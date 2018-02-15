{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Defunctionalization of typed, monomorphic Futhark programs without modules.
module Futhark.Internalise.Defunctionaliser
  ( transformProg
  , runDefM
  , defuncDecs
  ) where

import           Control.Monad.RWS
import           Data.List
import           Data.Loc
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           Futhark.MonadFreshNames
import           Language.Futhark

-- | A static value stores additional information about the result of
-- defunctionalization of an expression, aside from the residual expression.
data StaticVal = Dynamic CompType
               | LambdaSV Pattern Exp Env
               | RecordSV [(Name, StaticVal)]
               | DynamicFun (Exp, StaticVal) StaticVal
               | IntrinsicSV
  deriving (Show)

-- | Environment mapping variable names to their associated static value.
type Env = [(VName, StaticVal)]

emptyEnv :: Env
emptyEnv = []

extendEnv :: VName -> StaticVal -> Env -> Env
extendEnv vn sv = ((vn, sv) :)

combineEnv :: Env -> Env -> Env
combineEnv = (++)

-- | Restricts an environment to a given set of variable names.
-- Preserves the ordering of the mappings in the environment.
restrictEnv :: Names -> Env -> Env
restrictEnv names = filter ((`S.member` names) . fst)

-- | Defunctionalization monad.
newtype DefM a = DefM (RWS Env [Dec] VNameSource a)
  deriving (Functor, Applicative, Monad,
            MonadReader Env,
            MonadWriter [Dec],
            MonadFreshNames)

-- | Run a computation in the defunctionalization monad. Returns the result of
-- the computation, a new name source, and a list of lifted function declations.
runDefM :: VNameSource -> DefM a -> (a, VNameSource, [Dec])
runDefM src (DefM m) = runRWS m emptyEnv src

-- | Looks up the associated static value for a given name in the environment.
lookupVar :: SrcLoc -> VName -> DefM StaticVal
lookupVar loc x = do
  env <- ask
  case lookup x env of
    Just sv -> return sv
    Nothing -- If the variable is unknown, it may refer to the 'intrinsics'
            -- module, which we will have to treat specially.
      | baseTag x <= maxIntrinsicTag -> return IntrinsicSV
      | otherwise -> error $ "Variable " ++ pretty x ++ " at "
                          ++ locStr loc ++ " is out of scope."

-- | Defunctionalization of an expression. Returns the residual expression and
-- the associated static value in the defunctionalization monad.
defuncExp :: Exp -> DefM (Exp, StaticVal)

defuncExp e@Literal{} =
  return (e, Dynamic $ typeOf e)

defuncExp (Parens e loc) = do
  (e', sv) <- defuncExp e
  return (Parens e' loc, sv)

defuncExp (QualParens qn e loc) = do
  (e', sv) <- defuncExp e
  return (QualParens qn e' loc, sv)

defuncExp (TupLit es loc) = do
  (es', svs) <- unzip <$> mapM defuncExp es
  return (TupLit es' loc, RecordSV $ zip fields svs)
  where fields = map (nameFromString . show) [(1 :: Int) ..]

defuncExp (RecordLit fs loc) = do
  (fs', names_svs) <- unzip <$> mapM defuncField fs
  return (RecordLit fs' loc, RecordSV names_svs)

  where defuncField (RecordFieldExplicit vn e loc') = do
          (e', sv) <- defuncExp e
          return (RecordFieldExplicit vn e' loc', (vn, sv))
        defuncField (RecordFieldImplicit vn _ loc') = do
          sv <- lookupVar loc' vn
          case sv of
            -- If the implicit field refers to a dynamic function, we
            -- convert it to an explicit field with a record closing over
            -- the environment and bind the corresponding static value.
            DynamicFun (e, sv') _ -> let vn' = baseName vn
                                     in return (RecordFieldExplicit vn' e loc',
                                                (vn', sv'))
            -- The field may refer to a functional expression, so we get the
            -- type from the static value and not the one from the AST.
            _ -> let tp = Info $ typeFromSV sv
                 in return (RecordFieldImplicit vn tp loc', (baseName vn, sv))

defuncExp (ArrayLit es t@(Info t') loc) = do
  es' <- mapM defuncExp' es
  return (ArrayLit es' t loc, Dynamic t')

defuncExp (Range e1 me incl t@(Info t') loc) = do
  e1' <- defuncExp' e1
  me' <- mapM defuncExp' me
  incl' <- mapM defuncExp' incl
  return (Range e1' me' incl' t loc, Dynamic t')

defuncExp e@Empty{} =
  return (e, Dynamic $ typeOf e)

defuncExp (Var qn _ loc) = do
  sv <- lookupVar loc (qualLeaf qn)
  case sv of
    -- If the variable refers to a dynamic function, we return its closure
    -- representation (i.e., a record expression capturing the free variables
    -- and a 'LambdaSV' static value) instead of the variable itself.
    DynamicFun closure _ -> return closure
    _ -> let tp = typeFromSV sv
         in return (Var qn (Info ([], [], tp)) loc, sv)

defuncExp (Ascript e0 _ _) = defuncExp e0

defuncExp (LetPat tparams pat e1 e2 loc) = do
  let env_dim = envFromShapeParams tparams
  (e1', sv1) <- local (env_dim `combineEnv`) $ defuncExp e1
  let env  = matchPatternSV pat sv1
      pat' = updatePattern pat sv1
  (e2', sv2) <- local (env `combineEnv`) $ defuncExp e2
  return (LetPat tparams pat' e1' e2' loc, sv2)

defuncExp (LetFun vn (tparams, pats, _, rettype, e1) e2 loc) = do
  let env_dim = envFromShapeParams tparams
  (pats', e1', sv1) <- local (env_dim `combineEnv`) $ defuncLet pats e1 rettype
  (e2', sv2) <- local (extendEnv vn sv1) $ defuncExp e2
  case pats' of
    []  -> let t1 = vacuousShapeAnnotations $ typeOf e1'
           in return (LetPat [] (Id vn (Info t1) noLoc) e1' e2' loc, sv2)
    _:_ -> let t1 = vacuousShapeAnnotations . toStruct $ typeOf e1'
           in return (LetFun vn ([], pats', Nothing, Info t1, e1') e2' loc, sv2)

defuncExp (If e1 e2 e3 tp loc) = do
  (e1', _ ) <- defuncExp e1
  (e2', sv) <- defuncExp e2
  (e3', _ ) <- defuncExp e3
  return (If e1' e2' e3' tp loc, sv)

defuncExp e@Apply{} = defuncApply 0 e

defuncExp (Negate e0 loc) = do
  (e0', sv) <- defuncExp e0
  return (Negate e0' loc, sv)

defuncExp expr@(Lambda tparams pats e0 decl tp loc) = do
  when (any isTypeParam tparams) $ error $ expectedMonomorphic "lambda"
  -- Extract the first parameter of the lambda and "push" the
  -- remaining ones (if there are any) into the body of the lambda.
  let (pat, e0') = case pats of
        [] -> error "Received a lambda with no parameters."
        [pat'] -> (pat', e0)
        (pat' : pats') -> (pat', Lambda [] pats' e0 decl tp loc)

  env <- reader $ restrictEnv (freeVars expr)
  let fields = map (\(vn, sv) -> RecordFieldImplicit vn
                                 (Info $ typeFromSV sv) noLoc) env
  return (RecordLit fields loc, LambdaSV pat e0' env)

-- We leave the operator section expressions mostly unaffected for now,
-- assuming that they will only occur as function arguments to SOACs.
defuncExp e@OpSection{} = return (e, Dynamic $ typeOf e)

defuncExp expr@(OpSectionLeft qn e tps tp loc) = do
  e' <- defuncExp' e
  return (OpSectionLeft qn e' tps tp loc, Dynamic $ typeOf expr)

defuncExp expr@(OpSectionRight qn e tps tp loc) = do
  e' <- defuncExp' e
  return (OpSectionRight qn e' tps tp loc, Dynamic $ typeOf expr)

defuncExp (DoLoop tparams pat e1 form e3 loc) = do
  when (any isTypeParam tparams) $ error $ expectedMonomorphic "loop"
  (e1', sv1) <- defuncExp e1
  let env1 = matchPatternSV pat sv1
  (form', env2) <- case form of
    For ident e2  -> do (e2', sv2) <- defuncExp e2
                        return (For ident e2', [(identName ident, sv2)])
    ForIn pat2 e2 -> do (e2', sv2) <- defuncExp e2
                        return (ForIn pat2 e2', matchPatternSV pat2 sv2)
    While e2      -> do e2' <- defuncExp' e2
                        return (While e2', [])
  (e3', sv) <- local ((env1 `combineEnv` env2) `combineEnv`) $ defuncExp e3
  return (DoLoop tparams pat e1' form' e3' loc, sv)

defuncExp (BinOp qn (e1, d1) (e2, d2) t@(Info t') loc) = do
  e1' <- defuncExp' e1
  e2' <- defuncExp' e2
  return (BinOp qn (e1', d1) (e2', d2) t loc, Dynamic t')

defuncExp (Project vn e0 tp@(Info tp') loc) = do
  (e0', sv0) <- defuncExp e0
  case sv0 of
    RecordSV svs -> case lookup vn svs of
      Just sv -> return (Project vn e0' (Info $ typeFromSV sv) loc, sv)
      Nothing -> error "Invalid record projection."
    Dynamic _ -> return (Project vn e0' tp loc, Dynamic tp')
    _ -> error $ "Projection of an expression with static value " ++ show sv0

defuncExp (LetWith id1 id2 idxs e1 body loc) = do
  e1' <- defuncExp' e1
  sv1 <- lookupVar (identSrcLoc id2) $ identName id2
  idxs' <- mapM defuncDimIndex idxs
  (body', sv) <- local (extendEnv (identName id1) sv1) $ defuncExp body
  return (LetWith id1 id2 idxs' e1' body' loc, sv)

defuncExp expr@(Index e0 idxs loc) = do
  e0' <- defuncExp' e0
  idxs' <- mapM defuncDimIndex idxs
  return (Index e0' idxs' loc, Dynamic $ typeOf expr)

defuncExp (Update e1 idxs e2 loc) = do
  (e1', sv) <- defuncExp e1
  idxs' <- mapM defuncDimIndex idxs
  e2' <- defuncExp' e2
  return (Update e1' idxs' e2' loc, sv)

defuncExp e@(Concat i e1 es loc) = do
  e1' <- defuncExp' e1
  es' <- mapM defuncExp' es
  return (Concat i e1' es' loc, Dynamic $ typeOf e)

defuncExp e@(Reshape e1 e2 loc) = do
  e1' <- defuncExp' e1
  e2' <- defuncExp' e2
  return (Reshape e1' e2' loc, Dynamic $ typeOf e)

defuncExp e@(Rearrange is e0 loc) = do
  e0' <- defuncExp' e0
  return (Rearrange is e0' loc, Dynamic $ typeOf e)

defuncExp e@(Rotate i e1 e2 loc) = do
  e1' <- defuncExp' e1
  e2' <- defuncExp' e2
  return (Rotate i e1' e2' loc, Dynamic $ typeOf e)

defuncExp e@(Map fun es tp loc) = do
  fun' <- defuncSoacExp fun
  es' <- mapM defuncExp' es
  return (Map fun' es' tp loc, Dynamic $ typeOf e)

defuncExp e@(Reduce comm fun ne arr loc) = do
  fun' <- defuncSoacExp fun
  ne' <- defuncExp' ne
  arr' <- defuncExp' arr
  return (Reduce comm fun' ne' arr' loc, Dynamic $ typeOf e)

defuncExp e@(Scan fun ne arr loc) = do
  fun' <- defuncSoacExp fun
  ne' <- defuncExp' ne
  arr' <- defuncExp' arr
  return (Scan fun' ne' arr' loc, Dynamic $ typeOf e)

defuncExp e@(Filter fun arr loc) = do
  fun' <- defuncSoacExp fun
  arr' <- defuncExp' arr
  return (Filter fun' arr' loc, Dynamic $ typeOf e)

defuncExp e@(Partition funs arr loc) = do
  funs' <- mapM defuncSoacExp funs
  arr' <- defuncExp' arr
  return (Partition funs' arr' loc, Dynamic $ typeOf e)

defuncExp e@Stream{} = return (e, Dynamic $ typeOf e)

defuncExp e@(Zip i e1 es tp uniq loc) = do
  e1' <- defuncExp' e1
  es' <- mapM defuncExp' es
  return (Zip i e1' es' tp uniq loc, Dynamic $ typeOf e)

defuncExp e@(Unzip e0 tps loc) = do
  e0' <- defuncExp' e0
  return (Unzip e0' tps loc, Dynamic $ typeOf e)

defuncExp (Unsafe e1 loc) = do
  (e1', sv) <- defuncExp e1
  return (Unsafe e1' loc, sv)

-- | Same as 'defuncExp', except it ignores the static value.
defuncExp' :: Exp -> DefM Exp
defuncExp' = fmap fst . defuncExp

-- | Defunctionalize the function argument to a SOAC by eta-expanding if
-- necessary and then defunctionalizing the body of the introduced lambda.
defuncSoacExp :: Exp -> DefM Exp
defuncSoacExp e = do
  (pats, body, tp) <- etaExpand e
  let env = foldMap envFromPattern pats
  body' <- local (env `combineEnv`) $ defuncExp' body
  return $ Lambda [] pats body' Nothing (Info tp) noLoc

etaExpand :: Exp -> DefM ([Pattern], Exp, StructType)
etaExpand e = do
  let (ps, ret) = getType $ typeOf e
  (pats, vars) <- fmap unzip . forM ps $ \t -> do
    x <- newNameFromString "x"
    return (Id x (Info $ vacuousShapeAnnotations t) noLoc,
            Var (qualName x) (Info ([], [], t)) noLoc)
  let ps_st = map (vacuousShapeAnnotations . toStruct) ps
      e' = foldl' (\e1 (e2, t2, argtypes) ->
                     Apply e1 e2 (Info $ diet t2)
                     (Info (argtypes, ret)) noLoc)
           e $ zip3 vars ps (drop 1 $ tails ps_st)
  return (pats, e', vacuousShapeAnnotations $ toStruct ret)

  where getType (Arrow _ _ t1 t2) =
          let (ps, r) = getType t2 in (t1 : ps, r)
        getType t = ([], t)

-- | Defunctionalize an indexing of a single array dimension.
defuncDimIndex :: DimIndexBase Info VName -> DefM (DimIndexBase Info VName)
defuncDimIndex (DimFix e1) = DimFix . fst <$> defuncExp e1
defuncDimIndex (DimSlice me1 me2 me3) =
  DimSlice <$> defunc' me1 <*> defunc' me2 <*> defunc' me3
  where defunc' = mapM defuncExp'

-- | Defunctionalize a let-bound function, while preserving parameters
-- that have order 0 types (i.e., non-functional).
defuncLet :: [Pattern] -> Exp -> Info StructType -> DefM ([Pattern], Exp, StaticVal)
defuncLet ps@(pat:pats) body rettype
  | patternOrder pat == 0 = do
      let env = envFromPattern pat
      (pats', body', sv) <- local (env `combineEnv`) $
                            defuncLet pats body rettype
      closure <- defuncExp $ Lambda [] ps body Nothing rettype noLoc
      return (pat : pats', body', DynamicFun closure sv)
  | otherwise = do
      (e, sv) <- defuncExp $ Lambda [] ps body Nothing rettype noLoc
      return ([], e, sv)
defuncLet [] body _ = do
  (body', sv) <- defuncExp body
  return ([], body', sv)

-- | Defunctionalize an application expression at a given depth of application.
-- Calls to dynamic (first-order) functions are preserved at much as possible,
-- but a new lifted function is created if a dynamic function is only partially
-- applied.
defuncApply :: Int -> Exp -> DefM (Exp, StaticVal)
defuncApply depth e@(Apply e1 e2 d (Info (argtypes, _)) loc) = do
  (e1', sv1) <- defuncApply (depth+1) e1
  (e2', sv2) <- defuncExp e2
  case sv1 of
    LambdaSV pat e0 closure_env -> do
      let env' = matchPatternSV pat sv2
      (e0', sv) <- local (const $ combineEnv env' closure_env) $ defuncExp e0

      -- Lift lambda to top-level function definition.
      fname <- newNameFromString "lifted"
      let params = [ buildEnvPattern closure_env
                   , updatePattern pat sv2 ]
          rettype = typeOf e0'
      liftValDec fname rettype params e0'

      let t1 = vacuousShapeAnnotations . toStruct $ typeOf e1'
          t2 = vacuousShapeAnnotations . toStruct $ typeOf e2'
          fname' = qualName fname
      return (Parens (Apply (Apply (Var fname' (Info ([], [t1, t2], rettype)) loc)
                             e1' (Info Observe) (Info ([t2], rettype)) loc)
                      e2' d (Info ([], rettype)) loc) noLoc, sv)

    -- If e1 is a dynamic function, we just leave the application in place,
    -- but we update the types since it may be partially applied or return
    -- a higher-order term.
    DynamicFun _ sv ->
      let (argtypes', rettype) = dynamicFunType sv argtypes
      in return (Apply e1' e2' d (Info (argtypes', rettype)) loc, sv)

    -- Propagate the 'IntrinsicsSV' until we reach the outermost application,
    -- where we construct a dynamic static value with the appropriate type.
    IntrinsicSV
      | depth == 0 -> return (e, Dynamic $ typeOf e)
      | otherwise  -> return (e, IntrinsicSV)

    _ -> error $ "Application of an expression that is neither a static lambda "
              ++ "nor a dynamic function, but has static value: " ++ show sv1

defuncApply depth e@(Var qn (Info (_, argtypes, _)) loc) = do
    sv <- lookupVar loc (qualLeaf qn)
    case sv of
      DynamicFun _ _
        | fullyApplied sv depth ->
            -- We still need to update the types in case the dynamic
            -- function returns a higher-order term.
            let (argtypes', rettype) = dynamicFunType sv argtypes
            in return (Var qn (Info ([], argtypes', rettype)) loc, sv)

        | otherwise -> do
            fname <- newName $ qualLeaf qn
            let (pats, e0, sv') = liftDynFun sv depth
                sv'' = replNthDynFun sv sv' depth
                (argtypes', rettype) = dynamicFunType sv'' argtypes
            liftValDec fname rettype pats e0
            return (Var (qualName fname) (Info ([], argtypes', rettype)) loc, sv'')

      IntrinsicSV -> return (e, IntrinsicSV)

      _ -> return (Var qn (Info ([], [], typeFromSV sv)) loc, sv)

defuncApply _ expr = defuncExp expr

-- | Replace the n'th StaticVal in a sequence of DynamicFun's.
replNthDynFun :: StaticVal -> StaticVal -> Int -> StaticVal
replNthDynFun _ sv' 0 = sv'
replNthDynFun (DynamicFun clsr sv) sv' d
  | d > 0 = DynamicFun clsr $ replNthDynFun sv sv' (d-1)
replNthDynFun sv _ n = error $ "Tried to replace the " ++ show n
                             ++ "'th StaticVal in " ++ show sv

-- | Check if a 'StaticVal' and a given application depth corresponds
-- to a fully applied dynamic function.
fullyApplied :: StaticVal -> Int -> Bool
fullyApplied (DynamicFun _ sv) depth
  | depth == 0   = False
  | depth >  0   = fullyApplied sv (depth-1)
fullyApplied _ _ = True

-- | Converts a dynamic function 'StaticVal' into a list of parameters,
-- a function body, and the static value that results from applying the
-- function at the given depth of partial application.
liftDynFun :: StaticVal -> Int -> ([Pattern], Exp, StaticVal)
liftDynFun (DynamicFun (e, sv) _) 0 = ([], e, sv)
liftDynFun (DynamicFun (_, LambdaSV pat _ _) sv) d
  | d > 0 =  let (pats, e', sv') = liftDynFun sv (d-1)
             in (pat : pats, e', sv')
liftDynFun sv _ = error $ "Tried to lift a StaticVal " ++ show sv
                       ++ ", but expected a dynamic function."

-- | Converts a pattern to an environment that binds the individual names of the
-- pattern to their corresponding types wrapped in a 'Dynamic' static value.
envFromPattern :: Pattern -> Env
envFromPattern pat = case pat of
  TuplePattern ps _     -> foldMap envFromPattern ps
  RecordPattern fs _    -> foldMap (envFromPattern . snd) fs
  PatternParens p _     -> envFromPattern p
  Id vn (Info t) _      -> [(vn, Dynamic $ removeShapeAnnotations t)]
  Wildcard _ _          -> mempty
  PatternAscription p _ -> envFromPattern p

-- | Create an environment that binds the shape parameters.
envFromShapeParams :: [TypeParamBase VName] -> Env
envFromShapeParams = map envEntry
  where envEntry (TypeParamDim vn _)     = (vn, Dynamic . Prim $ Signed Int32)
        envEntry p@(TypeParamType _ loc) = error $
          "The defunctionalizer expects a monomorphic input program, but it\n" ++
          "received a type parameter " ++ pretty p ++ " at " ++ locStr loc ++ "."

-- | Create a new top-level value declaration with the given function name,
-- return type, list of parameters, and body expression.
liftValDec :: VName -> CompType -> [Pattern] -> Exp -> DefM ()
liftValDec fname rettype pats body = tell [dec]
  where rettype_st = vacuousShapeAnnotations $ toStruct rettype
        dec = ValDec ValBind
          { valBindEntryPoint = False
          , valBindName       = fname
          , valBindRetDecl    = Nothing
          , valBindRetType    = Info rettype_st
          , valBindTypeParams = []
          , valBindParams     = pats
          , valBindBody       = body
          , valBindDoc        = Nothing
          , valBindLocation   = noLoc
          }

-- | Given a closure environment, construct a record pattern that
-- binds the closed over variables.
buildEnvPattern :: Env -> Pattern
buildEnvPattern env = RecordPattern (map buildField env) noLoc
  where buildField (vn, sv) = let tp = vacuousShapeAnnotations $ typeFromSV sv
                              in (baseName vn, Id vn (Info tp) noLoc)

-- | Compute the corresponding type for a given static value.
typeFromSV :: StaticVal -> CompType
typeFromSV (Dynamic tp)           = tp
typeFromSV (LambdaSV _ _ env)     = typeFromEnv env
typeFromSV (RecordSV ls)          = Record . M.fromList $
                                    map (\(vn, sv) -> (vn, typeFromSV sv)) ls
typeFromSV (DynamicFun (_, sv) _) = typeFromSV sv
typeFromSV IntrinsicSV            = error $ "Tried to get the type from the "
                                         ++ "static value of an intrinsic."

typeFromEnv :: Env -> CompType
typeFromEnv = Record . M.fromList .
              map (\(vn, sv) -> (baseName vn, typeFromSV sv))

-- | Construct the type for a fully-applied dynamic function from its
-- static value and the original types of its arguments.
dynamicFunType :: StaticVal -> [StructType] -> ([StructType], CompType)
dynamicFunType (DynamicFun _ sv) (p:ps) =
  let (ps', ret) = dynamicFunType sv ps in (p : ps', ret)
dynamicFunType sv _ = ([], typeFromSV sv)

-- | Match a pattern with its static value. Returns an environment with
-- the identifier components of the pattern mapped to the corresponding
-- subcomponents of the static value.
matchPatternSV :: PatternBase f VName -> StaticVal -> Env
matchPatternSV (TuplePattern ps _) (RecordSV ls) =
  concat $ zipWith (\p (_, sv) -> matchPatternSV p sv) ps ls
matchPatternSV (RecordPattern ps _) (RecordSV ls)
  | ps' <- sortOn fst ps, ls' <- sortOn fst ls,
    map fst ps' == map fst ls' =
      concat $ zipWith (\(_, p) (_, sv) -> matchPatternSV p sv) ps' ls'
matchPatternSV (PatternParens pat _) sv = matchPatternSV pat sv
matchPatternSV (Id vn _ _) sv = [(vn, sv)]
matchPatternSV (Wildcard _ _) _ = []
matchPatternSV (PatternAscription pat _) sv = matchPatternSV pat sv
matchPatternSV pat (Dynamic t) = matchPatternSV pat $ svFromType t
matchPatternSV pat sv = error $ "Tried to match pattern " ++ pretty pat
                             ++ " with static value " ++ show sv ++ "."

-- | Given a pattern and the static value for the defunctionalized argument,
-- update the pattern to reflect the changes in the types.
updatePattern :: Pattern -> StaticVal -> Pattern
updatePattern (TuplePattern ps loc) (RecordSV svs) =
  TuplePattern (zipWith updatePattern ps $ map snd svs) loc
updatePattern (RecordPattern ps loc) (RecordSV svs)
  | ps' <- sortOn fst ps, svs' <- sortOn fst svs =
      RecordPattern (zipWith (\(n, p) (_, sv) ->
                                (n, updatePattern p sv)) ps' svs') loc
updatePattern (PatternParens pat loc) sv =
  PatternParens (updatePattern pat sv) loc
updatePattern (Id vn _ loc) sv =
  Id vn (Info . vacuousShapeAnnotations $ typeFromSV sv) loc
updatePattern (Wildcard _ loc) sv =
  Wildcard (Info . vacuousShapeAnnotations $ typeFromSV sv) loc
updatePattern (PatternAscription pat _) sv =
  updatePattern pat sv
updatePattern pat (Dynamic t) = updatePattern pat (svFromType t)
updatePattern pat sv =
  error $ "Tried to update pattern " ++ pretty pat
       ++ "to reflect the static value " ++ show sv

-- | Convert a record (or tuple) type to a record static value. This is used for
-- "unwrapping" tuples and records that are nested in 'Dynamic' static values.
svFromType :: CompType -> StaticVal
svFromType (Record fs) = RecordSV . M.toList $ M.map svFromType fs
svFromType t           = Dynamic t

-- | Compute the set of free variables of an expression.
freeVars :: Exp -> Names
freeVars expr = case expr of
  Literal{}            -> mempty
  Parens e _           -> freeVars e
  QualParens _ e _     -> freeVars e
  TupLit es _          -> foldMap freeVars es

  RecordLit fs _       -> foldMap freeVarsField fs
    where freeVarsField (RecordFieldExplicit _ e _)  = freeVars e
          freeVarsField (RecordFieldImplicit vn _ _) = S.singleton vn

  ArrayLit es _ _      -> foldMap freeVars es
  Range e me incl _ _  -> freeVars e <> foldMap freeVars me <>
                          foldMap freeVars incl
  Empty{}              -> mempty
  Var qn _ _           -> S.singleton $ qualLeaf qn
  Ascript e _ _        -> freeVars e
  LetPat _ pat e1 e2 _ -> freeVars e1 <> (freeVars e2 S.\\ patternVars pat)

  LetFun vn (_, pats, _, _, e1) e2 _ ->
    (freeVars e1 S.\\ foldMap patternVars pats) <>
    (freeVars e2 S.\\ S.singleton vn)

  If e1 e2 e3 _ _           -> freeVars e1 <> freeVars e2 <> freeVars e3
  Apply e1 e2 _ _ _         -> freeVars e1 <> freeVars e2
  Negate e _                -> freeVars e
  Lambda _ pats e0 _ _ _    -> freeVars e0 S.\\ foldMap patternVars pats
  OpSection{}               -> mempty
  OpSectionLeft  _ e _ _ _  -> freeVars e
  OpSectionRight _ e _ _ _  -> freeVars e

  DoLoop _ pat e1 form e3 _ -> let (e2fv, e2ident) = formVars form
                               in freeVars e1 <> e2fv <>
                               (freeVars e3 S.\\ (patternVars pat <> e2ident))
    where formVars (For ident e2) = (freeVars e2, S.singleton $ identName ident)
          formVars (ForIn p e2)   = (freeVars e2, patternVars p)
          formVars (While e2)     = (freeVars e2, S.empty)

  BinOp _ (e1, _) (e2, _) _ _  -> freeVars e1 <> freeVars e2
  Project _ e _ _              -> freeVars e

  LetWith id1 id2 idxs e1 e2 _ ->
    S.singleton (identName id2) <> foldMap freeDimIndex idxs <> freeVars e1 <>
    (freeVars e2 S.\\ S.singleton (identName id1))

  Index e idxs _      -> freeVars e  <> foldMap freeDimIndex idxs
  Update e1 idxs e2 _ -> freeVars e1 <> foldMap freeDimIndex idxs <> freeVars e2
  Concat _ e1 es _    -> freeVars e1 <> foldMap freeVars es
  Reshape e1 e2 _     -> freeVars e1 <> freeVars e2
  Rearrange _ e _     -> freeVars e
  Rotate _ e1 e2 _    -> freeVars e1 <> freeVars e2

  Map e1 es _ _       -> freeVars e1 <> foldMap freeVars es
  Reduce _ e1 e2 e3 _ -> freeVars e1 <> freeVars e2 <> freeVars e3
  Scan e1 e2 e3 _     -> freeVars e1 <> freeVars e2 <> freeVars e3
  Filter e1 e2 _      -> freeVars e1 <> freeVars e2
  Partition es e _    -> foldMap freeVars es <> freeVars e
  Stream form e1 e2 _ -> freeInForm form <> freeVars e1 <> freeVars e2
    where freeInForm (RedLike _ _ e) = freeVars e
          freeInForm _ = mempty

  Zip _ e es _ _ _    -> freeVars e <> foldMap freeVars es
  Unzip e _ _         -> freeVars e
  Unsafe e _          -> freeVars e

freeDimIndex :: DimIndexBase Info VName -> Names
freeDimIndex (DimFix e) = freeVars e
freeDimIndex (DimSlice me1 me2 me3) =
  foldMap (foldMap freeVars) [me1, me2, me3]

-- | Extract all the variable names bound in a pattern.
patternVars :: Pattern -> Names
patternVars (TuplePattern pats _)     = foldMap patternVars pats
patternVars (RecordPattern fs _)      = foldMap (patternVars . snd) fs
patternVars (PatternParens pat _)     = patternVars pat
patternVars (Id vn _ _)               = S.singleton vn
patternVars (Wildcard _ _)            = mempty
patternVars (PatternAscription pat _) = patternVars pat

expectedMonomorphic :: String -> String
expectedMonomorphic msg =
  "Received a " ++ msg ++ " with type parameters, but the \
  \defunctionalizer expects a monomorphic input program."

-- | Defunctionalize a top-level value binding. Returns the transformed result
-- as well as a function that extends the environment with a binding from the
-- bound name to the static value of the transformed body.
defuncValBind :: ValBind -> DefM (ValBind, Env -> Env)
defuncValBind valbind@(ValBind _ name _ rettype tparams params body _ _) = do
  let env = envFromShapeParams tparams
  (params', body', sv) <- local (env `combineEnv`) $
                          defuncLet params body rettype
  let rettype' = vacuousShapeAnnotations . toStruct $ typeOf body'
  return ( valbind { valBindRetDecl = Nothing
                   , valBindRetType = Info rettype'
                   , valBindParams  = params'
                   , valBindBody    = body'
                   }
         , extendEnv name sv)

defuncDecs :: [Dec] -> DefM [Dec]
defuncDecs [] = return []
defuncDecs (ValDec valbind : ds) = do
  (valbind', env) <- defuncValBind valbind
  ds' <- local env $ defuncDecs ds
  return $ ValDec valbind' : ds'
defuncDecs (TypeDec dec : ds) =
  (TypeDec dec :) <$> defuncDecs ds
defuncDecs (dec : _) =
  error $ "Defunctionalizer received declaration " ++ pretty dec
       ++ ", but can only handle value declarations at this point."

-- | Transform a list of top-level declarations. May produce new lifted function
-- definitions, which are placed in front of the resulting list of declarations.
transformProg :: MonadFreshNames m => [Dec] -> m [Dec]
transformProg decs = modifyNameSource $ \namesrc ->
  let (decs', namesrc', liftedDecs) = runDefM namesrc $ defuncDecs decs
  in (liftedDecs ++ decs', namesrc')
