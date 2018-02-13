-- | This monomorphization modules converts a well-typed, polymorphic,
-- module-free Futhark program into an equivalent monomorphic program.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futhark.Internalise.Monomorphiser
  ( transformProg
  , transformDecs
  , runMonoM
  ) where

import           Control.Monad.RWS
import           Control.Monad.State
import           Data.List
import           Data.Loc
import qualified Data.Map.Strict as M

import           Futhark.MonadFreshNames
import           Language.Futhark
import           Language.Futhark.Traversals
import           Language.Futhark.TypeChecker.Types

-- | The monomorphization monad reads 'PolyBinding's and writes 'MonoBinding's.
newtype PolyBinding = PolyBinding (VName, [TypeParam], [Pattern], StructType, Exp)
newtype MonoBinding = MonoBinding (VName, [Pattern], StructType, Exp)

type InstanceList = [TypeBase () ()]

-- | Monomorphization environment mapping names of polymorphic functions to a
-- representation of their corresponding function bindings.
type Env = M.Map VName PolyBinding

extendEnv :: VName -> PolyBinding -> Env -> Env
extendEnv = M.insert

-- | The monomorphization monad.
newtype MonoM a = MonoM (RWST Env [(VName, MonoBinding)] VNameSource
                          (State Lifts) a)
  deriving (Functor, Applicative, Monad,
            MonadReader Env,
            MonadWriter [(VName, MonoBinding)],
            MonadFreshNames)

runMonoM :: VNameSource -> MonoM a -> (a, VNameSource)
runMonoM src (MonoM m) = (a, src')
  where (a, src', _) = evalState (runRWST m mempty src) mempty

lookupVar :: VName -> MonoM PolyBinding
lookupVar vn = do
  env <- ask
  case M.lookup vn env of
    Just valbind -> return valbind
    Nothing -> error $ "Unknown variable " ++ pretty vn

-- | Mapping from function name and instance list to a new function name in case
-- the function has already been instantiated with those concrete types.
type Lifts = [((VName, InstanceList), VName)]

getLifts :: MonoM Lifts
getLifts = MonoM $ lift get

modifyLifts :: (Lifts -> Lifts) -> MonoM ()
modifyLifts = MonoM . lift . modify

addLifted :: VName -> InstanceList -> VName -> MonoM ()
addLifted fname il lifted_fname =
  modifyLifts (((fname, il), lifted_fname) :)

lookupLifted :: VName -> InstanceList -> MonoM (Maybe VName)
lookupLifted fname il = do
  lifts <- getLifts
  return $ lookup (fname, il) lifts


-- | Monomorphization of expressions.
transformExp :: Exp -> MonoM Exp
transformExp e@Literal{} = return e

transformExp (Parens e loc) =
  Parens <$> transformExp e <*> pure loc

transformExp (QualParens qn e loc) =
  QualParens qn <$> transformExp e <*> pure loc

transformExp (TupLit es loc) =
  TupLit <$> mapM transformExp es <*> pure loc

transformExp (RecordLit fs loc) =
  RecordLit <$> mapM transformField fs <*> pure loc
  where transformField (RecordFieldExplicit name e loc') =
          RecordFieldExplicit name <$> transformExp e <*> pure loc'
        transformField f@RecordFieldImplicit{} =
          return f  -- TODO: What if this is a polymorphic function?

transformExp (ArrayLit es tp loc) =
  ArrayLit <$> mapM transformExp es <*> pure tp <*> pure loc

transformExp (Range e1 me incl tp loc) = do
  e1' <- transformExp e1
  me' <- mapM transformExp me
  incl' <- mapM transformExp incl
  return $ Range e1' me' incl' tp loc

transformExp e@Empty{} = return e

transformExp e@(Var (QualName qs fname) (Info (il, ps, ret)) loc)
  | null il   = return e  -- If the instance list is empty, the variable
                          -- does not refer to a polymorphic function.
  | otherwise = do
      funbind <- lookupVar fname
      maybe_fname <- lookupLifted fname il
      fname' <- case maybe_fname of
                  Just x -> return x  -- The function has alread been
                                      -- monomorphized for the given il.
                  Nothing -> do
                    (fname', funbind') <- monomorphizeBinding funbind il
                    tell [(fname, funbind')]
                    addLifted fname il fname'
                    return fname'
      return $ Var (QualName qs fname') (Info (il, ps, ret)) loc

transformExp (Ascript e tp loc) =
  Ascript <$> transformExp e <*> pure tp <*> pure loc

transformExp (LetPat tparams pat e1 e2 loc) =
  LetPat tparams pat <$> transformExp e1 <*> transformExp e2 <*> pure loc

transformExp (LetFun fname (tparams, params, _, Info ret, body) e loc)
  | not (any isTypeParam tparams) = do
      body' <- transformExp body
      e' <- transformExp e
      return $ LetFun fname (tparams, params, Nothing, Info ret, body') e' loc

  | otherwise = do
      -- Retrieve the lifted monomorphic function bindings that are produced,
      -- filter those that are monomorphic versions of the current let-bound
      -- function and insert them at this point, and propagate the rest.
      let funbind = PolyBinding (fname, tparams, params, ret, body)
      pass $ do
        (e', bs) <- listen $ local (extendEnv fname funbind) $ transformExp e
        let (bs_local, bs_prop) = partition ((== fname) . fst) bs
        return (unfoldLetFuns (map snd bs_local) e', const bs_prop)

transformExp (If e1 e2 e3 tp loc) = do
  e1' <- transformExp e1
  e2' <- transformExp e2
  e3' <- transformExp e3
  return $ If e1' e2' e3' tp loc

transformExp (Apply e1 e2 d tp loc) = do
  e1' <- transformExp e1
  e2' <- transformExp e2
  return $ Apply e1' e2' d tp loc

transformExp (Negate e loc) =
  Negate <$> transformExp e <*> pure loc

transformExp (Lambda tparams params e0 decl tp loc) = do
  e0' <- transformExp e0
  return $ Lambda tparams params e0' decl tp loc

transformExp e@OpSection{} = return e

transformExp (OpSectionLeft qn e argtypes rettype loc) = do
  e' <- transformExp e
  return $ OpSectionLeft qn e' argtypes rettype loc

transformExp (OpSectionRight qn e argtypes rettype loc) = do
  e' <- transformExp e
  return $ OpSectionRight qn e' argtypes rettype loc

transformExp (DoLoop tparams pat e1 form e3 loc) = do
  e1' <- transformExp e1
  form' <- case form of
    For ident e2  -> For ident <$> transformExp e2
    ForIn pat2 e2 -> ForIn pat2 <$> transformExp e2
    While e2      -> While <$> transformExp e2
  e3' <- transformExp e3
  return $ DoLoop tparams pat e1' form' e3' loc

transformExp (BinOp qn (e1, d1) (e2, d2) tp loc) = do
  e1' <- transformExp e1
  e2' <- transformExp e2
  return $ BinOp qn (e1', d1) (e2', d2) tp loc

transformExp (Project n e tp loc) = do
  e' <- transformExp e
  return $ Project n e' tp loc

transformExp (LetWith id1 id2 idxs e1 body loc) = do
  idxs' <- mapM transformDimIndex idxs
  e1' <- transformExp e1
  body' <- transformExp body
  return $ LetWith id1 id2 idxs' e1' body' loc

transformExp (Index e0 idxs loc) =
  Index <$> transformExp e0 <*> mapM transformDimIndex idxs <*> pure loc

transformExp (Update e1 idxs e2 loc) =
  Update <$> transformExp e1 <*> mapM transformDimIndex idxs
         <*> transformExp e2 <*> pure loc

transformExp (Concat i e1 es loc) =
  Concat i <$> transformExp e1 <*> mapM transformExp es <*> pure loc

transformExp (Reshape e1 e2 loc) =
  Reshape <$> transformExp e1 <*> transformExp e2 <*> pure loc

transformExp (Rearrange is e0 loc) =
  Rearrange is <$> transformExp e0 <*> pure loc

transformExp (Rotate i e1 e2 loc) =
  Rotate i <$> transformExp e1 <*> transformExp e2 <*> pure loc

transformExp (Map e1 es tp loc) =
  Map <$> transformExp e1 <*> mapM transformExp es <*> pure tp <*> pure loc

transformExp (Reduce comm e1 e2 e3 loc) =
  Reduce comm <$> transformExp e1 <*> transformExp e2
              <*> transformExp e3 <*> pure loc

transformExp (Scan e1 e2 e3 loc) =
  Scan <$> transformExp e1 <*> transformExp e2 <*> transformExp e3 <*> pure loc

transformExp (Filter e1 e2 loc) =
  Filter <$> transformExp e1 <*> transformExp e2 <*> pure loc

transformExp (Partition es e0 loc) =
  Partition <$> mapM transformExp es <*> transformExp e0 <*> pure loc

transformExp (Stream form e1 e2 loc) = do
  form' <- case form of
             MapLike _         -> return form
             RedLike so comm e -> RedLike so comm <$> transformExp e
  Stream form' <$> transformExp e1 <*> transformExp e2 <*> pure loc

transformExp (Zip i e1 es tp uniq loc) = do
  e1' <- transformExp e1
  es' <- mapM transformExp es
  return $ Zip i e1' es' tp uniq loc

transformExp (Unzip e0 tps loc) =
  Unzip <$> transformExp e0 <*> pure tps <*> pure loc

transformExp (Unsafe e1 loc) =
  Unsafe <$> transformExp e1 <*> pure loc

transformDimIndex :: DimIndexBase Info VName -> MonoM (DimIndexBase Info VName)
transformDimIndex (DimFix e) = DimFix <$> transformExp e
transformDimIndex (DimSlice me1 me2 me3) =
  DimSlice <$> trans me1 <*> trans me2 <*> trans me3
  where trans = mapM transformExp

-- | Convert a collection of 'MonoBinding's to a nested sequence of let-bound,
-- monomorphic functions with the given expression at the bottom.
unfoldLetFuns :: [MonoBinding] -> Exp -> Exp
unfoldLetFuns [] e = e
unfoldLetFuns (MonoBinding (fname, params, rettype, body) : rest) e =
  LetFun fname ([], params, Nothing, Info rettype, body) e' noLoc
  where e' = unfoldLetFuns rest e

-- | Monomorphize a polymorphic function at the types given in the instance
-- list. Monomorphizes the body of the function as well. Returns the fresh name
-- of the generated monomorphic function and its 'MonoBinding' representation.
monomorphizeBinding :: PolyBinding -> InstanceList -> MonoM (VName, MonoBinding)
monomorphizeBinding (PolyBinding (name, tparams, params, rettype, body)) il = do
  let rettype' = substTypesAny substs_st rettype
      params' = map (substPattern $ substTypesAny substs_pt) params
  body' <- updateExpTypes body
  name' <- newName name
  body'' <- transformExp body'
  let monobind = MonoBinding (name', params', rettype', body'')
  return (name', monobind)

  where tnames = map typeParamName $ filter isTypeParam tparams
        substs = M.fromList $ zip tnames il
        substs_ct = M.map fromStruct substs
        substs_st = M.map vacuousShapeAnnotations substs
        substs_pt = M.map fromStruct substs_st

        updateExpTypes = astMap mapper
        mapper = ASTMapper { mapOnExp         = astMap mapper
                           , mapOnName        = pure
                           , mapOnQualName    = pure
                           , mapOnType        = pure . substTypesAny substs
                           , mapOnCompType    = pure . substTypesAny substs_ct
                           , mapOnStructType  = pure . substTypesAny substs_st
                           , mapOnPatternType = pure . substTypesAny substs_pt
                           }

-- | Perform a given substitution on the types in a pattern.
substPattern :: (PatternType -> PatternType) -> Pattern -> Pattern
substPattern f pat = case pat of
  TuplePattern pats loc  -> TuplePattern (map (substPattern f) pats) loc
  RecordPattern fs loc   -> RecordPattern (map substField fs) loc
    where substField (n, p) = (n, substPattern f p)
  PatternParens p loc    -> PatternParens (substPattern f p) loc
  Id vn (Info tp) loc    -> Id vn (Info $ f tp) loc
  Wildcard (Info tp) loc -> Wildcard (Info $ f tp) loc
  PatternAscription p _  -> substPattern f p

toPolyBinding :: ValBind -> PolyBinding
toPolyBinding (ValBind _ name _ (Info rettype) tparams params body _ _) =
  PolyBinding (name, tparams, params, rettype, body)

toValBinding :: MonoBinding -> ValBind
toValBinding (MonoBinding (name, params, rettype, body)) =
  ValBind { valBindEntryPoint = False
          , valBindName       = name
          , valBindRetDecl    = Nothing
          , valBindRetType    = Info rettype
          , valBindTypeParams = []
          , valBindParams     = params
          , valBindBody       = body
          , valBindDoc        = Nothing
          , valBindLocation   = noLoc
          }

-- | Transform a list of top-level declarations.
transformDecs :: [Dec] -> MonoM [Dec]
transformDecs [] = return []
transformDecs (ValDec valbind : ds)
  | not $ any isTypeParam $ valBindTypeParams valbind = pass $ do
      (body', binds) <- listen $ transformExp $ valBindBody valbind
      (ds', rem_binds) <- listen $ transformDecs ds
      let valbind' = ValDec valbind { valBindBody = body' }
          valbinds = map (ValDec . toValBinding . snd) binds
      return (valbinds ++ [valbind'] ++ ds', const rem_binds)

  | otherwise = pass $ do
      (ds', binds) <- listen $ local (extendEnv (valBindName valbind)
                                                (toPolyBinding valbind))
                                     (transformDecs ds)
      let valbinds = map (ValDec . toValBinding . snd) binds
      return (valbinds ++ ds', const mempty)
transformDecs (TypeDec typebind : ds) = do
  ds' <- transformDecs ds
  return $ TypeDec typebind : ds'
transformDecs (dec : _) =
  error $ "The monomorphization module expects a module-free input program"
       ++ ", but received: " ++ pretty dec

transformProg :: MonadFreshNames m => [Dec] -> m [Dec]
transformProg decs = modifyNameSource $ \namesrc ->
  runMonoM namesrc $ transformDecs decs
