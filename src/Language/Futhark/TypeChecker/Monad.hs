{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, TupleSections #-}
-- | Main monad in which the type checker runs, as well as ancillary
-- data definitions.
module Language.Futhark.TypeChecker.Monad
  ( TypeError(..)
  , TypeM
  , runTypeM
  , askEnv
  , askRootEnv
  , localTmpEnv
  , recursionPermitted
  , checkQualNameWithEnv
  , bindSpaced
  , qualifyTypeVars
  , getType

  , MonadTypeChecker(..)
  , checkName
  , badOnLeft

  , Warnings

  , Env(..)
  , TySet
  , FunSig(..)
  , ImportTable
  , NameMap
  , BoundV(..)
  , Mod(..)
  , TypeBinding(..)
  , MTy(..)

  , anySignedType
  , anyUnsignedType
  , anyIntType
  , anyFloatType
  , anyNumberType
  , anyPrimType

  , Namespace(..)
  , intrinsicsNameMap
  , topLevelNameMap
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS.Strict
import Control.Monad.Identity
import Data.List
import Data.Loc
import Data.Maybe
import Data.Either
import Data.Ord
import Data.Hashable
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Semigroup as Sem
import qualified System.FilePath.Posix as Posix

import Prelude hiding (mapM, mod)

import Language.Futhark
import Language.Futhark.Traversals
import Futhark.FreshNames hiding (newName)
import qualified Futhark.FreshNames

-- | Information about an error during type checking.  The 'Show'
-- instance for this type produces a human-readable description.
data TypeError =
    TypeError SrcLoc String
  | UnifyError SrcLoc (TypeBase () ()) SrcLoc (TypeBase () ())
  | UnexpectedType SrcLoc
    (TypeBase () ()) [TypeBase () ()]
  | ReturnTypeError SrcLoc Name (TypeBase () ()) (TypeBase () ())
  | DupDefinitionError Namespace Name SrcLoc SrcLoc
  | DupPatternError Name SrcLoc SrcLoc
  | InvalidPatternError (PatternBase NoInfo Name)
    (TypeBase (DimDecl Name) ()) (Maybe String) SrcLoc
  | UnknownVariableError Namespace (QualName Name) SrcLoc
  | ParameterMismatch (Maybe (QualName Name)) SrcLoc
    (Either Int [TypeBase () ()]) [TypeBase () ()]
  | UseAfterConsume Name SrcLoc SrcLoc
  | ConsumeAfterConsume Name SrcLoc SrcLoc
  | IndexingError Int Int SrcLoc
  | CurriedConsumption (QualName Name) SrcLoc
  | BadLetWithValue SrcLoc
  | ReturnAliased Name Name SrcLoc
  | UniqueReturnAliased Name SrcLoc
  | PermutationError SrcLoc [Int] Int
  | DimensionNotInteger SrcLoc (QualName Name)
  | InvalidUniqueness SrcLoc (TypeBase () ())
  | UndefinedType SrcLoc (QualName Name)
  | InvalidField SrcLoc CompType String
  | UnderscoreUse SrcLoc (QualName Name)
  | ValueIsNotFunction SrcLoc (QualName Name) CompType
  | FunctionIsNotValue SrcLoc (QualName Name)
  | UniqueConstType SrcLoc Name (TypeBase () ())
  | UndeclaredFunctionReturnType SrcLoc (QualName Name)
  | UnappliedFunctor SrcLoc

instance Show TypeError where
  show (TypeError pos msg) =
    "Type error at " ++ locStr pos ++ ":\n" ++ msg
  show (UnifyError e1loc t1 e2loc t2) =
    "Cannot unify type " ++ pretty t1 ++
    " of expression at " ++ locStr e1loc ++
    "\nwith type " ++ pretty t2 ++
    " of expression at " ++ locStr e2loc
  show (UnexpectedType loc _ []) =
    "Type of expression at " ++ locStr loc ++
    "cannot have any type - possibly a bug in the type checker."
  show (UnexpectedType loc t ts) =
    "Type of expression at " ++ locStr loc ++ " must be one of " ++
    intercalate ", " (map pretty ts) ++ ", but is " ++
    pretty t ++ "."
  show (ReturnTypeError pos fname rettype bodytype) =
    "Declaration of function " ++ nameToString fname ++ " at " ++ locStr pos ++
    " declares return type " ++ pretty rettype ++ ", but body has type " ++
    pretty bodytype
  show (DupDefinitionError space name pos1 pos2) =
    "Duplicate definition of " ++ ppSpace space ++ " " ++ nameToString name ++ ".  Defined at " ++
    locStr pos1 ++ " and " ++ locStr pos2 ++ "."
  show (DupPatternError name pos1 pos2) =
    "Duplicate binding of '" ++ pretty name ++ "'; at " ++
    locStr pos1 ++ " and " ++ locStr pos2 ++ "."
  show (InvalidPatternError pat t desc loc) =
    "Pattern " ++ pretty pat ++
    " cannot match value of type " ++ pretty t ++ " at " ++ locStr loc ++ end
    where end = case desc of Nothing -> "."
                             Just desc' -> ":\n" ++ desc'
  show (UnknownVariableError space name pos) =
    "Unknown " ++ ppSpace space ++ " " ++ pretty name ++ " referenced at " ++ locStr pos ++ "."
  show (ParameterMismatch fname pos expected got) =
    "In call of " ++ fname' ++ " at position " ++ locStr pos ++ ":\n" ++
    "expecting " ++ show nexpected ++ " argument(s) of type(s) " ++
     expected' ++ ", but got " ++ show ngot ++
    " arguments of types " ++ intercalate ", " (map pretty got) ++ "."
    where (nexpected, expected') =
            case expected of
              Left i -> (i, "(polymorphic)")
              Right ts -> (length ts, intercalate ", " $ map pretty ts)
          ngot = length got
          fname' = maybe "anonymous function" (("function "++) . pretty) fname
  show (UseAfterConsume name rloc wloc) =
    "Variable " ++ pretty name ++ " used at " ++ locStr rloc ++
    ", but it was consumed at " ++ locStr wloc ++ ".  (Possibly through aliasing)"
  show (ConsumeAfterConsume name loc1 loc2) =
    "Variable " ++ pretty name ++ " consumed at both " ++ locStr loc1 ++
    " and " ++ locStr loc2 ++ ".  (Possibly through aliasing)"
  show (IndexingError dims got pos) =
    show got ++ " indices given at " ++ locStr pos ++
    ", but type of indexee  has " ++ show dims ++ " dimension(s)."
  show (CurriedConsumption fname loc) =
    "Function " ++ pretty fname ++
    " curried over a consuming parameter at " ++ locStr loc ++ "."
  show (BadLetWithValue loc) =
    "New value for elements in let-with shares data with source array at " ++
    locStr loc ++ ".  This is illegal, as it prevents in-place modification."
  show (ReturnAliased fname name loc) =
    "Unique return value of function " ++ nameToString fname ++ " at " ++
    locStr loc ++ " is aliased to " ++ pretty name ++ ", which is not consumed."
  show (UniqueReturnAliased fname loc) =
    "A unique tuple element of return value of function " ++
    nameToString fname ++ " at " ++ locStr loc ++
    " is aliased to some other tuple component."
  show (PermutationError loc perm r) =
    "The permutation (" ++ intercalate ", " (map show perm) ++
    ") is not valid for array argument of rank " ++ show r ++ " at " ++
    locStr loc ++ "."
  show (DimensionNotInteger loc name) =
    "Dimension declaration " ++ pretty name ++ " at " ++ locStr loc ++
    " should be an integer."
  show (InvalidUniqueness loc t) =
    "Attempt to declare unique non-array " ++ pretty t ++ " at " ++ locStr loc ++ "."
  show (UndefinedType loc name) =
    "Unknown type " ++ pretty name ++ " referenced at " ++ locStr loc ++ "."
  show (InvalidField loc t field) =
    "Attempt to access field '" ++ field ++ "' of value of type " ++
    pretty t ++ " at " ++ locStr loc ++ "."
  show (UnderscoreUse loc name) =
    "Use of " ++ pretty name ++ " at " ++ locStr loc ++
    ": variables prefixed with underscore must not be accessed."
  show (ValueIsNotFunction loc name t) =
    "Attempt to use value " ++ pretty name ++ " of type " ++ pretty t ++
    " as function at " ++ locStr loc ++ "."
  show (FunctionIsNotValue loc name) =
    "Attempt to use function " ++ pretty name ++ " as value at " ++ locStr loc ++ "."
  show (UniqueConstType loc name t) =
    "Constant " ++ pretty name ++ " defined with unique type " ++ pretty t ++ " at " ++
    locStr loc ++ ", which is not allowed."
  show (UndeclaredFunctionReturnType loc fname) =
    "Function '" ++ pretty fname ++ "' with no return type declaration called at " ++
    locStr loc
  show (UnappliedFunctor loc) =
    "Cannot have parametric module at " ++ locStr loc ++ "."

-- | A set of abstract types and where their definition is expected.
type TySet = S.Set (QualName VName)

-- | Representation of a module, which is either a plain environment,
-- or a parametric module ("functor" in SML).
data Mod = ModEnv Env
         | ModFun FunSig
         deriving (Show)

-- | A parametric functor consists of a set of abstract types, the
-- environment of its parameter, and the resulting module type.
data FunSig = FunSig { funSigAbs :: TySet
                     , funSigMod :: Mod
                     , funSigMty :: MTy
                     }
            deriving (Show)

-- | Representation of a module type.
data MTy = MTy { mtyAbs :: TySet
                 -- ^ Abstract types in the module type.
               , mtyMod :: Mod
               }
         deriving (Show)

-- | A binding from a name to its definition as a type.
data TypeBinding = TypeAbbr [TypeParam] StructType
                 deriving (Eq, Show)

-- | Type parameters, list of parameter types (optinally named), and
-- return type.  The type parameters are in scope in both parameter
-- types and the return type.  Non-functional values have only a
-- return type.
data BoundV = BoundV [TypeParam] StructType
                deriving (Show)

type NameMap = M.Map (Namespace, Name) (QualName VName)

-- | Modules produces environment with this representation.
data Env = Env { envVtable :: M.Map VName BoundV
               , envTypeTable :: M.Map VName TypeBinding
               , envSigTable :: M.Map VName MTy
               , envModTable :: M.Map VName Mod
               , envNameMap :: NameMap
               } deriving (Show)

instance Sem.Semigroup Env where
  Env vt1 tt1 st1 mt1 nt1 <> Env vt2 tt2 st2 mt2 nt2 =
    Env (vt1<>vt2) (tt1<>tt2) (st1<>st2) (mt1<>mt2) (nt1<>nt2)

instance Monoid Env where
  mempty = Env mempty mempty mempty mempty mempty
  mappend = (Sem.<>)

-- | The warnings produced by the type checker.  The 'Show' instance
-- produces a human-readable description.
newtype Warnings = Warnings [(SrcLoc, String)]

instance Sem.Semigroup Warnings where
  Warnings ws1 <> Warnings ws2 = Warnings $ ws1 <> ws2

instance Monoid Warnings where
  mempty = Warnings mempty
  mappend = (Sem.<>)

instance Show Warnings where
  show (Warnings []) = ""
  show (Warnings ws) =
    intercalate "\n\n" ws' ++ "\n"
    where ws' = map showWarning $ sortBy (comparing (off . locOf . fst)) ws
          off NoLoc = 0
          off (Loc p _) = posCoff p
          showWarning (loc, w) =
            "Warning at " ++ locStr loc ++ ":\n  " ++ w

singleWarning :: SrcLoc -> String -> Warnings
singleWarning loc problem = Warnings [(loc, problem)]

type ImportTable = M.Map FilePath Env

data Context = Context { contextEnv :: Env
                       , contextRootEnv :: Env
                       , contextImportTable :: ImportTable
                       , contextFilePath :: FilePath
                       , contextPermitRecursion :: Bool
                       }

-- | The type checker runs in this monad.
newtype TypeM a = TypeM (RWST
                         Context -- Reader
                         Warnings           -- Writer
                         VNameSource        -- State
                         (Except TypeError) -- Inner monad
                         a)
  deriving (Monad, Functor, Applicative,
            MonadReader Context,
            MonadWriter Warnings,
            MonadState VNameSource,
            MonadError TypeError)

runTypeM :: Bool -> Env -> ImportTable -> FilePath -> VNameSource
         -> TypeM a
         -> Either TypeError (a, Warnings, VNameSource)
runTypeM recurse env imports fpath src (TypeM m) = do
  (x, src', ws) <- runExcept $ runRWST m (Context env env imports fpath recurse) src
  return (x, ws, src')

askEnv, askRootEnv :: TypeM Env
askEnv = asks contextEnv
askRootEnv = asks contextRootEnv

localTmpEnv :: Env -> TypeM a -> TypeM a
localTmpEnv env = local $ \ctx ->
  ctx { contextEnv = env <> contextEnv ctx }

recursionPermitted :: TypeM Bool
recursionPermitted = asks contextPermitRecursion

class MonadError TypeError m => MonadTypeChecker m where
  warn :: SrcLoc -> String -> m ()

  newName :: VName -> m VName
  newID :: Name -> m VName

  bindNameMap :: NameMap -> m a -> m a
  localEnv :: Env -> m a -> m a

  checkQualName :: Namespace -> QualName Name -> SrcLoc -> m (QualName VName)

  lookupType :: SrcLoc -> QualName Name -> m (QualName VName, [TypeParam], StructType)
  lookupMod :: SrcLoc -> QualName Name -> m (QualName VName, Mod)
  lookupMTy :: SrcLoc -> QualName Name -> m (QualName VName, MTy)
  lookupImport :: SrcLoc -> FilePath -> m (FilePath, Env)
  lookupVar :: SrcLoc -> QualName Name -> m (QualName VName, [TypeBase () ()], CompType)
  -- ^ Also returns the instance list for the type parameters, in case
  -- this variables refers to a polymorphic function.

checkName :: MonadTypeChecker m => Namespace -> Name -> SrcLoc -> m VName
checkName space name loc = qualLeaf <$> checkQualName space (qualName name) loc

bindSpaced :: MonadTypeChecker m => [(Namespace, Name)] -> m a -> m a
bindSpaced names body = do
  names' <- mapM (newID . snd) names
  let mapping = M.fromList (zip names $ map qualName names')
  bindNameMap mapping body

instance MonadTypeChecker TypeM where
  warn loc problem = tell $ singleWarning loc problem

  newName s = do src <- get
                 let (s', src') = Futhark.FreshNames.newName src s
                 put src'
                 return s'

  newID s = newName $ VName s 0

  bindNameMap m = local $ \ctx ->
    let env = contextEnv ctx
    in ctx { contextEnv = env { envNameMap = m <> envNameMap env } }

  localEnv env = local $ \ctx ->
    let env' = env <> contextEnv ctx
    in ctx { contextEnv = env', contextRootEnv = env' }

  checkQualName space name loc = snd <$> checkQualNameWithEnv space name loc

  lookupType loc qn = do
    outer_env <- askRootEnv
    (scope, qn'@(QualName qs name)) <- checkQualNameWithEnv Type qn loc
    case M.lookup name $ envTypeTable scope of
      Nothing -> throwError $ UndefinedType loc qn
      Just (TypeAbbr ps def) -> return (qn', ps, qualifyTypeVars outer_env mempty qs def)

  lookupMod loc qn = do
    (scope, qn'@(QualName _ name)) <- checkQualNameWithEnv Term qn loc
    case M.lookup name $ envModTable scope of
      Nothing -> throwError $ UnknownVariableError Term qn loc
      Just m  -> return (qn', m)

  lookupMTy loc qn = do
    (scope, qn'@(QualName _ name)) <- checkQualNameWithEnv Signature qn loc
    (qn',) <$> maybe explode return (M.lookup name $ envSigTable scope)
    where explode = throwError $ UnknownVariableError Signature qn loc

  lookupImport loc file = do
    imports <- asks contextImportTable
    my_path <- asks contextFilePath
    let abs_path = my_path Posix.</> file
    case M.lookup abs_path imports of
      Nothing    -> throwError $ TypeError loc $ "Unknown import \"" ++ file ++ "\"" ++ extra
      Just scope -> return (abs_path, scope)
      where extra | ".." `elem` Posix.splitDirectories file =
                      "\nNote: '..' is not supported in file imports."
                  | otherwise =
                      ""

  lookupVar loc qn = do
    outer_env <- askRootEnv
    (env, qn'@(QualName qs name)) <- checkQualNameWithEnv Term qn loc
    case M.lookup name $ envVtable env of
      Nothing -> throwError $ UnknownVariableError Term qn loc
      Just (BoundV _ t)
        | "_" `isPrefixOf` pretty name -> throwError $ UnderscoreUse loc qn
        | otherwise -> do
            case getType t of
              Left{} -> throwError $ FunctionIsNotValue loc qn
              Right t' -> return (qn', [], removeShapeAnnotations $ fromStruct $
                                           qualifyTypeVars outer_env mempty qs t')

-- | Extract from a type either a function type comprising a list of
-- parameter types and a return type, or a first-order type.
getType :: TypeBase dim as
        -> (Either ([(Maybe VName, TypeBase dim as)], TypeBase dim as)
                   (TypeBase dim as))
getType (Arrow _ v t1 t2) =
  case getType t2 of
    Left (ps, r) -> Left ((v, t1) : ps, r)
    Right _ -> Left ([(v, t1)], t2)
getType t = Right t

checkQualNameWithEnv :: Namespace -> QualName Name -> SrcLoc -> TypeM (Env, QualName VName)
checkQualNameWithEnv space qn@(QualName quals name) loc = do
  env <- askEnv
  descend env quals
  where descend scope []
          | Just name' <- M.lookup (space, name) $ envNameMap scope =
              return (scope, name')
          | otherwise =
              throwError $ UnknownVariableError space qn loc

        descend scope (q:qs)
          | Just (QualName _ q') <- M.lookup (Term, q) $ envNameMap scope,
            Just res <- M.lookup q' $ envModTable scope =
              case res of
                ModEnv q_scope -> do
                  (scope', QualName qs' name') <- descend q_scope qs
                  return (scope', QualName (q':qs') name')
                ModFun{} -> throwError $ UnappliedFunctor loc
          | otherwise =
              throwError $ UnknownVariableError space qn loc

-- Try to prepend qualifiers to the type names such that they
-- represent how to access the type in some scope.
qualifyTypeVars :: ASTMappable t => Env -> [VName] -> [VName] -> t -> t
qualifyTypeVars outer_env except qs = runIdentity . astMap mapper
  where mapper = ASTMapper { mapOnExp = pure
                           , mapOnName = pure
                           , mapOnQualName = pure . qual
                           , mapOnType = pure
                           , mapOnCompType = pure
                           , mapOnStructType = pure
                           , mapOnPatternType = pure
                           }
        qual (QualName orig_qs name)
          | name `elem` except ||
            reachable orig_qs name outer_env = QualName orig_qs name
          | otherwise                        = QualName (qs<>orig_qs) name

        reachable [] name env =
          isJust $ find matches $ M.elems (envTypeTable env)
          where matches (TypeAbbr [] (TypeVar (TypeName x_qs name') [])) =
                  null x_qs && name == name'
                matches _ = False

        reachable (q:qs') name env
          | Just (ModEnv env') <- M.lookup q $ envModTable env =
              reachable qs' name env'
          | otherwise = False

badOnLeft :: MonadTypeChecker m => Either TypeError a -> m a
badOnLeft = either throwError return

anySignedType :: [TypeBase () ()]
anySignedType = map (Prim . Signed) [minBound .. maxBound]

anyUnsignedType :: [TypeBase () ()]
anyUnsignedType = map (Prim . Unsigned) [minBound .. maxBound]

anyIntType :: [TypeBase () ()]
anyIntType = anySignedType ++ anyUnsignedType

anyFloatType :: [TypeBase () ()]
anyFloatType = map (Prim . FloatType) [minBound .. maxBound]

anyNumberType :: [TypeBase () ()]
anyNumberType = anyIntType ++ anyFloatType

anyPrimType :: [TypeBase () ()]
anyPrimType = Prim Bool : anyIntType ++ anyFloatType

--- Name handling

data Namespace = Term -- ^ Functions and values.
               | Type
               | Signature
               deriving (Eq, Ord, Show, Enum)

ppSpace :: Namespace -> String
ppSpace Term = "value"
ppSpace Type = "type"
ppSpace Signature = "module type"

instance Hashable Namespace where
  hashWithSalt salt = hashWithSalt salt . fromEnum

intrinsicsNameMap :: NameMap
intrinsicsNameMap = M.fromList $ map mapping $ M.toList intrinsics
  where mapping (v, IntrinsicType{}) = ((Type, baseName v), QualName [mod] v)
        mapping (v, _)               = ((Term, baseName v), QualName [mod] v)
        mod = VName (nameFromString "intrinsics") 0

topLevelNameMap :: NameMap
topLevelNameMap = M.filterWithKey (\k _ -> atTopLevel k) intrinsicsNameMap
  where atTopLevel :: (Namespace, Name) -> Bool
        atTopLevel (Type, _) = True
        atTopLevel (Term, v) = v `S.member` (type_names <> binop_names <> unop_names <> fun_names)
          where type_names = S.fromList $ map (nameFromString . pretty) anyPrimType
                binop_names = S.fromList $ map (nameFromString . pretty)
                              [minBound..(maxBound::BinOp)]
                unop_names = S.fromList $ map nameFromString ["~", "!"]
                fun_names = S.fromList $ map nameFromString ["shape"]
        atTopLevel _         = False
