{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, LambdaCase #-}
-- | A generic C# code generator which is polymorphic in the type
-- of the operations.  Concretely, we use this to handle both
-- sequential and OpenCL C# code.
module Futhark.CodeGen.Backends.GenericCSharp
  ( compileProg
  , Constructor (..)
  , emptyConstructor

  , compileName
  , compileDim
  , compileExp
  , compileCode
  , compilePrimValue
  , compilePrimType
  , compilePrimTypeExt

  , Operations (..)
  , defaultOperations

  , unpackDim

  , CompilerM (..)
  , OpCompiler
  , WriteScalar
  , ReadScalar
  , Allocate
  , Copy
  , StaticArray
  , EntryOutput
  , EntryInput

  , CompilerEnv(..)
  , CompilerState(..)
  , stm
  , stms
  , atInit
  , collect'
  , collect
  , simpleCall

  , copyMemoryDefaultSpace
  ) where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as M

import Futhark.Representation.Primitive hiding (Bool)
import Futhark.MonadFreshNames
import Futhark.Representation.AST.Syntax (Space(..))
import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.CodeGen.Backends.GenericCSharp.AST
import Futhark.CodeGen.Backends.GenericCSharp.Options
import Futhark.CodeGen.Backends.GenericCSharp.Definitions
import Futhark.Util.Pretty(pretty)
import Futhark.Util (zEncodeString)
import Futhark.Representation.AST.Attributes (builtInFunctions, isBuiltInFunction)

-- | A substitute expression compiler, tried before the main
-- compilation function.
type OpCompiler op s = op -> CompilerM op s ()

-- | Write a scalar to the given memory block with the given index and
-- in the given memory space.
type WriteScalar op s = VName -> CSExp -> PrimType -> Imp.SpaceId -> CSExp
                        -> CompilerM op s ()

-- | Read a scalar from the given memory block with the given index and
-- in the given memory space.
type ReadScalar op s = VName -> CSExp -> PrimType -> Imp.SpaceId
                       -> CompilerM op s CSExp

-- | Allocate a memory block of the given size in the given memory
-- space, saving a reference in the given variable name.
type Allocate op s = VName -> CSExp -> Imp.SpaceId
                     -> CompilerM op s ()

-- | Copy from one memory block to another.
type Copy op s = VName -> CSExp -> Imp.Space ->
                 VName -> CSExp -> Imp.Space ->
                 CSExp -> PrimType ->
                 CompilerM op s ()

-- | Create a static array of values - initialised at load time.
type StaticArray op s = VName -> Imp.SpaceId -> PrimType -> [PrimValue] -> CompilerM op s ()

-- | Construct the CSthon array being returned from an entry point.
type EntryOutput op s = VName -> Imp.SpaceId ->
                        PrimType -> Imp.Signedness ->
                        [Imp.DimSize] ->
                        CompilerM op s CSExp

-- | Unpack the array being passed to an entry point.
type EntryInput op s = VName -> Imp.MemSize -> Imp.SpaceId ->
                       PrimType -> Imp.Signedness ->
                       [Imp.DimSize] ->
                       CSExp ->
                       CompilerM op s ()


data Operations op s = Operations { opsWriteScalar :: WriteScalar op s
                                  , opsReadScalar :: ReadScalar op s
                                  , opsAllocate :: Allocate op s
                                  , opsCopy :: Copy op s
                                  , opsStaticArray :: StaticArray op s
                                  , opsCompiler :: OpCompiler op s
                                  , opsEntryOutput :: EntryOutput op s
                                  , opsEntryInput :: EntryInput op s
                                  }

-- | A set of operations that fail for every operation involving
-- non-default memory spaces.  Uses plain pointers and @malloc@ for
-- memory management.
defaultOperations :: Operations op s
defaultOperations = Operations { opsWriteScalar = defWriteScalar
                               , opsReadScalar = defReadScalar
                               , opsAllocate  = defAllocate
                               , opsCopy = defCopy
                               , opsStaticArray = defStaticArray
                               , opsCompiler = defCompiler
                               , opsEntryOutput = defEntryOutput
                               , opsEntryInput = defEntryInput
                               }
  where defWriteScalar _ _ _ _ _ =
          fail "Cannot write to non-default memory space because I am dumb"
        defReadScalar _ _ _ _ =
          fail "Cannot read from non-default memory space"
        defAllocate _ _ _ =
          fail "Cannot allocate in non-default memory space"
        defCopy _ _ _ _ _ _ _ _ =
          fail "Cannot copy to or from non-default memory space"
        defStaticArray _ _ _ _ =
          fail "Cannot create static array in non-default memory space"
        defCompiler _ =
          fail "The default compiler cannot compile extended operations"
        defEntryOutput _ _ _ _ =
          fail "Cannot return array not in default memory space"
        defEntryInput _ _ _ _ =
          fail "Cannot accept array not in default memory space"

data CompilerEnv op s = CompilerEnv {
    envOperations :: Operations op s
  , envFtable     :: M.Map Name [Imp.Type]
}

envOpCompiler :: CompilerEnv op s -> OpCompiler op s
envOpCompiler = opsCompiler . envOperations

envReadScalar :: CompilerEnv op s -> ReadScalar op s
envReadScalar = opsReadScalar . envOperations

envWriteScalar :: CompilerEnv op s -> WriteScalar op s
envWriteScalar = opsWriteScalar . envOperations

envAllocate :: CompilerEnv op s -> Allocate op s
envAllocate = opsAllocate . envOperations

envCopy :: CompilerEnv op s -> Copy op s
envCopy = opsCopy . envOperations

envStaticArray :: CompilerEnv op s -> StaticArray op s
envStaticArray = opsStaticArray . envOperations

envEntryOutput :: CompilerEnv op s -> EntryOutput op s
envEntryOutput = opsEntryOutput . envOperations

envEntryInput :: CompilerEnv op s -> EntryInput op s
envEntryInput = opsEntryInput . envOperations

newCompilerEnv :: Imp.Functions op -> Operations op s -> CompilerEnv op s
newCompilerEnv (Imp.Functions funs) ops =
  CompilerEnv { envOperations = ops
              , envFtable = ftable <> builtinFtable
              }
  where ftable = M.fromList $ map funReturn funs
        funReturn (name, Imp.Function _ outparams _ _ _ _) = (name, paramsTypes outparams)
        builtinFtable = M.map (map Imp.Scalar . snd) builtInFunctions

data CompilerState s = CompilerState {
    compNameSrc :: VNameSource
  , compInit :: [CSStmt]
  , compUserState :: s
}

newCompilerState :: VNameSource -> s -> CompilerState s
newCompilerState src s = CompilerState { compNameSrc = src
                                       , compInit = []
                                       , compUserState = s }

newtype CompilerM op s a = CompilerM (RWS (CompilerEnv op s) [CSStmt] (CompilerState s) a)
  deriving (Functor, Applicative, Monad,
            MonadState (CompilerState s),
            MonadReader (CompilerEnv op s),
            MonadWriter [CSStmt])

instance MonadFreshNames (CompilerM op s) where
  getNameSource = gets compNameSrc
  putNameSource src = modify $ \s -> s { compNameSrc = src }

collect :: CompilerM op s () -> CompilerM op s [CSStmt]
collect m = pass $ do
  ((), w) <- listen m
  return (w, const mempty)

collect' :: CompilerM op s a -> CompilerM op s (a, [CSStmt])
collect' m = pass $ do
  (x, w) <- listen m
  return ((x, w), const mempty)

atInit :: CSStmt -> CompilerM op s ()
atInit x = modify $ \s ->
  s { compInit = compInit s ++ [x] }

stm :: CSStmt -> CompilerM op s ()
stm x = tell [x]

stms :: [CSStmt] -> CompilerM op s ()
stms = mapM_ stm

futharkFun :: String -> String
futharkFun s = "futhark_" ++ zEncodeString s

paramsTypes :: [Imp.Param] -> [Imp.Type]
paramsTypes = map paramType
  where paramType (Imp.MemParam _ space) = Imp.Mem (Imp.ConstSize 0) space
        paramType (Imp.ScalarParam _ t) = Imp.Scalar t

compileOutput :: [Imp.Param] -> [(CSExp, CSType)]
compileOutput params = zip (map nameFun params) (map typeFun params)
  where nameFun = (Var . compileName . Imp.paramName)
        typeFun = (Primitive . compileType . paramType)

--runCompilerM :: Imp.Functions op -> Operations op s
--             -> VNameSource
--             -> s
--             -> CompilerM op s a
--             -> a
--runCompilerM prog ops src userstate (CompilerM m) =
--  fst $ evalRWS m (newCompilerEnv prog ops) (newCompilerState src userstate)

runCompilerM :: Imp.Functions op -> Operations op s
             -> VNameSource
             -> s
             -> CompilerM op s a
             -> a
runCompilerM prog ops src userstate (CompilerM m) =
  fst $ evalRWS m (newCompilerEnv prog ops) (newCompilerState src userstate)

standardOptions :: [Option]
standardOptions = [
  Option { optionLongName = "write-runtime-to"
         , optionShortName = Just 't'
         , optionArgument = RequiredArgument
         , optionAction =
           [
             If (Var "runtime_file")
             [Exp $ simpleCall "runtime_file.close" []] []
           , Assign (Var "runtime_file") $
             simpleCall "open" [Var "optarg", String "w"]
           ]
         },
  Option { optionLongName = "runs"
         , optionShortName = Just 'r'
         , optionArgument = RequiredArgument
         , optionAction =
           [ Assign (Var "num_runs") $ Var "optarg"
           , Assign (Var "do_warmup_run") $ Bool True
           ]
         },
  Option { optionLongName = "entry-point"
         , optionShortName = Just 'e'
         , optionArgument = RequiredArgument
         , optionAction =
           [ Assign (Var "entry_point") $ Var "optarg" ]
         },
  -- The -b option is just a dummy for now.
  Option { optionLongName = "binary-output"
         , optionShortName = Just 'b'
         , optionArgument = NoArgument
         , optionAction = [Pass]
         }
  ]

-- | The class generated by the code generator must have a
-- constructor, although it can be vacuous.
data Constructor = Constructor [CSFunDefArg] [CSStmt]

-- | A constructor that takes no arguments and does nothing.
emptyConstructor :: Constructor
emptyConstructor = Constructor [] []

constructorToConstructorDef :: Constructor -> String -> [CSStmt] -> CSStmt
constructorToConstructorDef (Constructor params body) name at_init =
  ConstructorDef $ ClassConstructor name params $ body <> at_init


compileProg :: MonadFreshNames m =>
               Maybe String
            -> Constructor
            -> [CSStmt]
            -> [CSStmt]
            -> Operations op s
            -> s
            -> [CSStmt]
            -> [Option]
            -> Imp.Functions op
            -> m String
compileProg module_name constructor imports defines ops userstate pre_timing options prog@(Imp.Functions funs) = do
  src <- getNameSource
  let prog' = runCompilerM prog ops src userstate compileProg'
  return $ pretty (CSProg $
                    imports ++
                    defines ++
                    [Escape csUtility] ++
                    prog')
  where compileProg' = do
          definitions <- mapM compileFunc funs
          at_inits <- gets compInit

  -- TODO: add #define DEBUG so we have assertions
  -- TODO: top up with usings
  -- TODO: import System.Diagnostics
  -- TODO: include CS code from /rts/cs
          case module_name of
            Just name -> do
              let constructor' = constructorToConstructorDef constructor name at_inits
              return [ClassDef $ Class name $ constructor' : map FunDef definitions]
            Nothing -> do
              let name = "internal"
              let constructor' = constructorToConstructorDef constructor name at_inits
              return (parse_options ++
                      [ClassDef $ Class name $ constructor' : map FunDef definitions]
                     )
        parse_options = []
--        parse_options =
--          Assign (Var "runtime_file") None :
--          Assign (Var "do_warmup_run") (Bool False) :
--          Assign (Var "num_runs") (Integer 1) :
--          Assign (Var "entry_point") (String "main") :
--          generateOptionParser (standardOptions ++ options)

compileFunc :: (Name, Imp.Function op) -> CompilerM op s CSFunDef
compileFunc (fname, Imp.Function _ outputs inputs body _ _) = do
  body' <- collect $ compileCode body
  let inputs' = map compileTypedInput inputs
  let outputs' = compileOutput outputs
  let (ret, retType) = unzip outputs'
  let retType' = tupleOrSingleT retType
  let ret' = Return $ tupleOrSingle ret

  return $ Def (futharkFun . nameToString $ fname) retType' inputs' (body'++[ret'])
  where compileTypedInput :: Imp.Param -> (String, CSType)
        compileTypedInput input = (nameFun input, typeFun input)
        nameFun = (compileName . Imp.paramName)
        typeFun = (Primitive . compileType . paramType)

tupleOrSingle :: [CSExp] -> CSExp
tupleOrSingle [e] = e
tupleOrSingle es = Tuple es

tupleOrSingleT :: [CSType] -> CSType
tupleOrSingleT [e] = e
tupleOrSingleT es = Composite $ TupleT es

-- | A 'Call' where the function is a variable and every argument is a
-- simple 'Arg'.
simpleCall :: String -> [CSExp] -> CSExp
simpleCall fname = Call (Var fname) . map simpleArg

simpleArg :: CSExp -> CSArg
simpleArg = Arg Nothing

-- | A CallMethod
callMethod :: CSExp -> String -> [CSExp] -> CSExp
callMethod object method = CallMethod object (Var method) . map simpleArg

simpleInitClass :: String -> [CSExp] -> CSExp
simpleInitClass fname = CreateObject (Var fname) . map simpleArg

compileName :: VName -> String
compileName = zEncodeString . pretty

compileType :: PrimType -> CSPrim
compileType (IntType Int8) = CSInt Int8T
compileType (IntType Int16) = CSInt Int16T
compileType (IntType Int32) = CSInt Int32T
compileType (IntType Int64) = CSInt Int64T
compileType (FloatType Float32) = CSFloat FloatT
compileType (FloatType Float64) = CSFloat DoubleT
compileType Imp.Bool = BoolT
compileType _ = CSInt Int8T

compileDim :: Imp.DimSize -> CSExp
compileDim (Imp.ConstSize i) = Integer $ toInteger i
compileDim (Imp.VarSize v) = Var $ compileName v

unpackDim :: CSExp -> Imp.DimSize -> Int32 -> CompilerM op s ()
unpackDim arr_name (Imp.ConstSize c) i = do
  let shape_name = Field arr_name "shape"
  let constant_c = Integer $ toInteger c
  let constant_i = Integer $ toInteger i
  stm $ Assert (BinOp "==" constant_c (Index shape_name $ IdxExp constant_i)) "constant dimension wrong"

unpackDim arr_name (Imp.VarSize var) i = do
  let shape_name = Field arr_name "shape"
  let src = Index shape_name $ IdxExp $ Integer $ toInteger i
  let dest = Var $ compileName var
  let makeNumpy = simpleCall "np.int32" [src]
  stm $ Try [Assert (BinOp "==" dest makeNumpy) "variant dimension wrong"]
        [Catch (Var "NameError") [Assign dest makeNumpy]]

entryPointOutput :: Imp.ExternalValue -> CompilerM op s CSExp
entryPointOutput (Imp.OpaqueValue desc vs) =
  simpleCall "opaque" . (String (pretty desc):) <$>
  mapM (entryPointOutput . Imp.TransparentValue) vs
entryPointOutput (Imp.TransparentValue (Imp.ScalarValue bt ept name)) =
  return $ simpleCall tf [Var $ compileName name]
  where tf = compilePrimToExtCs bt ept
entryPointOutput (Imp.TransparentValue (Imp.ArrayValue mem _ Imp.DefaultSpace bt ept dims)) = do
  let cast = Cast (Var $ compileName mem) (compilePrimTypeExt bt ept)
  return $ simpleCall "createArray" [cast, Tuple $ map compileDim dims]
entryPointOutput (Imp.TransparentValue (Imp.ArrayValue mem _ (Imp.Space sid) bt ept dims)) = do
  pack_output <- asks envEntryOutput
  pack_output mem sid bt ept dims

-- TODO
badInput :: Int -> CSExp -> String -> CSStmt
badInput i e t =
  Throw $ simpleInitClass "TypeError"
  [Call (Field (String err_msg) "format") [simpleArg (String t), simpleArg e]]
  where err_msg = unlines [ "Argument #" ++ show i ++ " has invalid value"
                          , "Futhark type: {}"
                          , "Type of given CSthon value: {}" ]


entryPointInput :: (Int, Imp.ExternalValue, CSExp) -> CompilerM op s ()
entryPointInput (i, Imp.OpaqueValue desc vs, e) = do
  let type_is_ok = BinOp "and" (simpleCall "isinstance" [e, Var "opaque"])
                               (BinOp "==" (Field e "desc") (String desc))
  stm $ If (UnOp "not" type_is_ok) [badInput i (simpleCall "type" [e]) desc] []
  mapM_ entryPointInput $ zip3 (repeat i) (map Imp.TransparentValue vs) $
    map (Index (Field e "data") . IdxExp . Integer) [0..]

entryPointInput (i, Imp.TransparentValue (Imp.ScalarValue bt s name), e) = do
  let vname' = Var $ compileName name
      -- HACK: A Numpy int64 will signal an OverflowError if we pass
      -- it a number bigger than 2**63.  This does not happen if we
      -- pass e.g. int8 a number bigger than 2**7.  As a workaround,
      -- we first go through the corresponding ctypes type, which does
      -- not have this problem.
      ctobject = compilePrimType bt
      ctcall = simpleCall ctobject [e]
      npobject = compilePrimToCs bt
      npcall = simpleCall npobject [ctcall]
  stm $ Try [Assign vname' npcall]
    [Catch (Tuple [Var "TypeError", Var "AssertionError"])
     [badInput i (simpleCall "type" [e]) $ prettySigned (s==Imp.TypeUnsigned) bt]]

entryPointInput (i, Imp.TransparentValue (Imp.ArrayValue mem memsize Imp.DefaultSpace t s dims), e) = do
  let type_is_wrong =
        UnOp "not" $
        BinOp "and"
        (BinOp "in" (simpleCall "type" [e]) (Array [Var "np.ndarray"]))
        (BinOp "==" (Field e "dtype") (Var (compilePrimToExtCs t s)))
  stm $ If type_is_wrong
    [badInput i (simpleCall "type" [e]) $ concat (replicate (length dims) "[]") ++
     prettySigned (s==Imp.TypeUnsigned) t]
    []

  zipWithM_ (unpackDim e) dims [0..]
  let dest = Var $ compileName mem
      unwrap_call = simpleCall "unwrapArray" [e]

  case memsize of
    Imp.VarSize sizevar ->
      stm $ Assign (Var $ compileName sizevar) $
      simpleCall "np.int32" [Field e "nbytes"]
    Imp.ConstSize _ ->
      return ()

  stm $ Assign dest unwrap_call

entryPointInput (i, Imp.TransparentValue (Imp.ArrayValue mem memsize (Imp.Space sid) bt ept dims), e) = do
  unpack_input <- asks envEntryInput
  unpack <- collect $ unpack_input mem memsize sid bt ept dims e
  stm $ Try unpack
    [Catch (Tuple [Var "TypeError", Var "AssertionError"])
     [badInput i (simpleCall "type" [e]) $ concat (replicate (length dims) "[]") ++
     prettySigned (ept==Imp.TypeUnsigned) bt]]

copyMemoryDefaultSpace :: VName -> CSExp -> VName -> CSExp -> CSExp ->
                          CompilerM op s ()
copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes = do
  let offset_call1 = simpleCall "addressOffset"
                     [Var (compileName destmem), destidx, Var "ct.c_byte"]
  let offset_call2 = simpleCall "addressOffset"
                     [Var (compileName srcmem), srcidx, Var "ct.c_byte"]
  stm $ Exp $ simpleCall "ct.memmove" [offset_call1, offset_call2, nbytes]

extValueDescName :: Imp.ExternalValue -> String
extValueDescName (Imp.TransparentValue v) = extName $ valueDescName v
extValueDescName (Imp.OpaqueValue desc []) = extName $ zEncodeString desc
extValueDescName (Imp.OpaqueValue desc (v:_)) =
  extName $ zEncodeString desc ++ "_" ++ pretty (baseTag (valueDescVName v))

extName :: String -> String
extName = (++"_ext")

valueDescName :: Imp.ValueDesc -> String
valueDescName = compileName . valueDescVName

valueDescVName :: Imp.ValueDesc -> VName
valueDescVName (Imp.ScalarValue _ _ vname) = vname
valueDescVName (Imp.ArrayValue vname _ _ _ _ _) = vname

readFun :: PrimType -> Imp.Signedness -> String
readFun (FloatType Float32) _ = "read_f32"
readFun (FloatType Float64) _ = "read_f64"
readFun (IntType Int8)  Imp.TypeUnsigned = "read_u8"
readFun (IntType Int16) Imp.TypeUnsigned = "read_u16"
readFun (IntType Int32) Imp.TypeUnsigned = "read_u32"
readFun (IntType Int64) Imp.TypeUnsigned = "read_u64"
readFun (IntType Int8)  Imp.TypeDirect   = "read_i8"
readFun (IntType Int16) Imp.TypeDirect   = "read_i16"
readFun (IntType Int32) Imp.TypeDirect   = "read_i32"
readFun (IntType Int64) Imp.TypeDirect   = "read_i64"
readFun Imp.Bool _      = "read_bool"
readFun Cert _          = error "readFun: cert"

-- The value returned will be used when reading binary arrays, to indicate what
-- the expected type is
readTypeEnum :: PrimType -> Imp.Signedness -> String
readTypeEnum (IntType Int8)  Imp.TypeUnsigned = "FUTHARK_UINT8"
readTypeEnum (IntType Int16) Imp.TypeUnsigned = "FUTHARK_UINT16"
readTypeEnum (IntType Int32) Imp.TypeUnsigned = "FUTHARK_UINT32"
readTypeEnum (IntType Int64) Imp.TypeUnsigned = "FUTHARK_UINT64"
readTypeEnum (IntType Int8)  Imp.TypeDirect   = "FUTHARK_INT8"
readTypeEnum (IntType Int16) Imp.TypeDirect   = "FUTHARK_INT16"
readTypeEnum (IntType Int32) Imp.TypeDirect   = "FUTHARK_INT32"
readTypeEnum (IntType Int64) Imp.TypeDirect   = "FUTHARK_INT64"
readTypeEnum (FloatType Float32) _ = "FUTHARK_FLOAT32"
readTypeEnum (FloatType Float64) _ = "FUTHARK_FLOAT64"
readTypeEnum Imp.Bool _ = "FUTHARK_BOOL"
readTypeEnum Cert _ = error "readTypeEnum: cert"

readInput :: Imp.ExternalValue -> CSStmt
readInput (Imp.OpaqueValue desc _) =
  Throw $ simpleInitClass "Exception" [String $ "Cannot read argument of type " ++ desc ++ "."]

readInput decl@(Imp.TransparentValue (Imp.ScalarValue bt ept _)) =
  let reader' = readFun bt ept
      stdin = Var "input_stream"
  in Assign (Var $ extValueDescName decl) $ simpleCall reader' [stdin]

-- TODO: If the type identifier of 'Float32' is changed, currently the error
-- messages for reading binary input will not use this new name. This is also a
-- problem for the C runtime system.
readInput decl@(Imp.TransparentValue (Imp.ArrayValue _ _ _ bt ept dims)) =
  let rank' = Var $ show $ length dims
      type_enum = Var $ readTypeEnum bt ept
      ct = Var $ compilePrimToExtCs bt ept
      stdin = Var "input_stream"
  in Assign (Var $ extValueDescName decl) $ simpleCall "read_array"
     [stdin, type_enum, rank', ct]

printPrimStm :: CSExp -> PrimType -> Imp.Signedness -> CSStmt
printPrimStm val t ept =
  case (t, ept) of
    (IntType Int8, Imp.TypeUnsigned) -> p "%uu8"
    (IntType Int16, Imp.TypeUnsigned) -> p "%uu16"
    (IntType Int32, Imp.TypeUnsigned) -> p "%uu32"
    (IntType Int64, Imp.TypeUnsigned) -> p "%uu64"
    (IntType Int8, _) -> p "%di8"
    (IntType Int16, _) -> p "%di16"
    (IntType Int32, _) -> p "%di32"
    (IntType Int64, _) -> p "%di64"
    (Imp.Bool, _) -> If val
                     [Exp $ simpleCall "sys.stdout.write" [String "true"]]
                     [Exp $ simpleCall "sys.stdout.write" [String "false"]]
    (Cert, _) -> Exp $ simpleCall "sys.stdout.write" [String "Checked"]
    (FloatType Float32, _) -> p "%.6ff32"
    (FloatType Float64, _) -> p "%.6ff64"
  where p s =
          Exp $ simpleCall "sys.stdout.write"
          [BinOp "%" (String s) val]

printStm :: Imp.ValueDesc -> CSExp -> CompilerM op s CSStmt
printStm (Imp.ScalarValue bt ept _) e =
  return $ printPrimStm e bt ept
printStm (Imp.ArrayValue _ _ _ bt ept []) e =
  return $ printPrimStm e bt ept
printStm (Imp.ArrayValue mem memsize space bt ept (outer:shape)) e = do
  v <- newVName "print_elem"
  first <- newVName "print_first"
  let size = simpleCall "np.product" [Array $ map compileDim $ outer:shape]
      emptystr = "empty(" ++ ppArrayType bt (length shape) ++ ")"
  printelem <- printStm (Imp.ArrayValue mem memsize space bt ept shape) $ Var $ compileName v
  return $ If (BinOp "==" size (Integer 0))
    [puts emptystr]
    [Assign (Var $ pretty first) $ Var "True",
     puts "[",
     For (pretty v) e [
        If (simpleCall "not" [Var $ pretty first])
        [puts ", "] [],
        printelem,
        Assign (Var $ pretty first) $ Var "False"
    ],
    puts "]"]
    where ppArrayType :: PrimType -> Int -> String
          ppArrayType t 0 = prettyPrimType ept t
          ppArrayType t n = "[]" ++ ppArrayType t (n-1)

          prettyPrimType Imp.TypeUnsigned (IntType Int8) = "u8"
          prettyPrimType Imp.TypeUnsigned (IntType Int16) = "u16"
          prettyPrimType Imp.TypeUnsigned (IntType Int32) = "u32"
          prettyPrimType Imp.TypeUnsigned (IntType Int64) = "u64"
          prettyPrimType _ t = pretty t

          puts s = Exp $ simpleCall "sys.stdout.write" [String s]

printValue :: [(Imp.ExternalValue, CSExp)] -> CompilerM op s [CSStmt]
printValue = fmap concat . mapM (uncurry printValue')
  -- We copy non-host arrays to the host before printing.  This is
  -- done in a hacky way - we assume the value has a .get()-method
  -- that returns an equivalent Numpy array.  This works for CSOpenCL,
  -- but we will probably need yet another plugin mechanism here in
  -- the future.
  where printValue' (Imp.OpaqueValue desc _) _ =
          return [Exp $ simpleCall "sys.stdout.write"
                  [String $ "#<opaque " ++ desc ++ ">"]]
        printValue' (Imp.TransparentValue (Imp.ArrayValue mem memsize (Space _) bt ept shape)) e =
          printValue' (Imp.TransparentValue (Imp.ArrayValue mem memsize DefaultSpace bt ept shape)) $
          simpleCall (pretty e ++ ".get") []
        printValue' (Imp.TransparentValue r) e = do
          p <- printStm r e
          return [p, Exp $ simpleCall "sys.stdout.write" [String "\n"]]

addTiming :: [CSStmt] -> ([CSStmt], CSStmt)
addTiming statements =
  ([ Assign (Var "stop_watch") $ simpleInitClass "Stopwatch" []
   , Exp $ simpleCall "stop_watch.Start" [] ] ++
   statements ++
   [ Exp $ simpleCall "stop_watch.Stop" []
   , Assign (Var "time_elapsed") $ Field (Var "stop_watch") "ElapsedMillisconds"
   , If (Var "runtime_file") print_runtime [] ],
   If (Var "runtime_file") [Exp $ simpleCall "runtime_file.Close" []] []
  )
  where stop_watch =
          "stop_watch"
        print_runtime =
          [Exp $ simpleCall "runtime_file.WriteLine"
           [ callMethod (toMicroseconds (Var "time_elapsed")) "ToString" [] ],
           Exp $ simpleCall "runtime_file.WriteLine" [String "\n"]]
        toMicroseconds x =
          BinOp "*" x $ Integer 1000000

compileUnOp :: Imp.UnOp -> String
compileUnOp op =
  case op of
    Not -> "!"
    Complement{} -> "~"
    Abs{} -> "MathUtils.Abs" -- actually write these helpers
    FAbs{} -> "MathUtils.Abs"
    SSignum{} -> "MathUtils.SSignum"
    USignum{} -> "MathUtils.USignum"

compileBinOpLike :: Monad m =>
                    Imp.Exp -> Imp.Exp
                 -> CompilerM op s (CSExp, CSExp, String -> m CSExp)
compileBinOpLike x y = do
  x' <- compileExp x
  y' <- compileExp y
  let simple s = return $ BinOp s x' y'
  return (x', y', simple)

-- | The ctypes type corresponding to a 'PrimType'.
compilePrimType :: PrimType -> String
compilePrimType t =
  case t of
    IntType Int8 -> "byte"
    IntType Int16 -> "short"
    IntType Int32 -> "int"
    IntType Int64 -> "long"
    FloatType Float32 -> "float"
    FloatType Float64 -> "double"
    Imp.Bool -> "bool"
    Cert -> "bool"

-- | The ctypes type corresponding to a 'PrimType', taking sign into account.
compilePrimTypeExt :: PrimType -> Imp.Signedness -> String
compilePrimTypeExt t ept =
  case (t, ept) of
    (IntType Int8, Imp.TypeUnsigned) -> "byte"
    (IntType Int16, Imp.TypeUnsigned) -> "ushort"
    (IntType Int32, Imp.TypeUnsigned) -> "uint"
    (IntType Int64, Imp.TypeUnsigned) -> "ulong"
    (IntType Int8, _) -> "sbyte"
    (IntType Int16, _) -> "short"
    (IntType Int32, _) -> "int"
    (IntType Int64, _) -> "long"
    (FloatType Float32, _) -> "float"
    (FloatType Float64, _) -> "double"
    (Imp.Bool, _) -> "bool"
    (Cert, _) -> "byte"

-- | The ctypes type corresponding to a 'PrimType'.
compilePrimToCs :: PrimType -> String
compilePrimToCs t =
  case t of
    IntType Int8 -> "Convert.ToByte"
    IntType Int16 -> "Convert.ToInt16"
    IntType Int32 -> "Convert.ToInt32"
    IntType Int64 -> "Convert.ToInt64"
    FloatType Float32 -> "Convert.ToSingle"
    FloatType Float64 -> "Convert.ToDouble"
    Imp.Bool -> "Convert.ToBool"
    Cert -> "Convert.ToByte"

-- | Select function to retrieve bytes from byte array as specific data type
compileBitConverter :: PrimType -> String
compileBitConverter t =
  case t of
    IntType Int8 -> undefined
    IntType Int16 -> "BitConverter.ToInt16"
    IntType Int32 -> "BitConverter.ToInt32"
    IntType Int64 -> "BitConverter.ToInt64"
    FloatType Float32 -> "BitConverter.ToSingle"
    FloatType Float64 -> "BitConverter.ToDouble"
    Imp.Bool -> "BitConverter.ToBool"
    Cert -> undefined

-- | The ctypes type corresponding to a 'PrimType', taking sign into account.
compilePrimToExtCs :: PrimType -> Imp.Signedness -> String
compilePrimToExtCs t ept =
  case (t, ept) of
    (IntType Int8, Imp.TypeUnsigned) -> "Convert.ToByte"
    (IntType Int16, Imp.TypeUnsigned) -> "Convert.ToUInt16"
    (IntType Int32, Imp.TypeUnsigned) -> "Convert.ToUInt32"
    (IntType Int64, Imp.TypeUnsigned) -> "Convert.ToUInt64"
    (IntType Int8 , _) -> "Convert.ToSByte"
    (IntType Int16 , _) -> "Convert.ToInt16"
    (IntType Int32 , _) -> "Convert.ToInt32"
    (IntType Int64 , _) -> "Convert.ToInt64"
    (FloatType Float32 , _) -> "Convert.ToSingle"
    (FloatType Float64 , _) -> "Convert.ToDouble"
    (Imp.Bool , _) -> "Convert.ToBool"
    (Cert , _) -> "Convert.ToByte"

compilePrimValue :: Imp.PrimValue -> CSExp
compilePrimValue (IntValue (Int8Value v)) =
  simpleCall "Convert.ToByte" [Integer $ toInteger v]
compilePrimValue (IntValue (Int16Value v)) =
  simpleCall "Convert.ToInt16" [Integer $ toInteger v]
compilePrimValue (IntValue (Int32Value v)) =
  simpleCall "Convert.ToInt32" [Integer $ toInteger v]
compilePrimValue (IntValue (Int64Value v)) =
  simpleCall "Convert.ToInt64" [Integer $ toInteger v]
compilePrimValue (FloatValue (Float32Value v))
  | isInfinite v =
      if v > 0 then Var "Single.PositiveInfinity" else Var "Single.NegativeInfinity"
  | isNaN v =
      Var "Single.NaN"
  | otherwise = simpleCall "Convert.ToSingle" [Float $ fromRational $ toRational v]
compilePrimValue (FloatValue (Float64Value v))
  | isInfinite v =
      if v > 0 then Var "Double.PositiveInfinity" else Var "Double.NegativeInfinity"
  | isNaN v =
      Var "Double.NaN"
  | otherwise = simpleCall "Convert.ToDouble" [Float $ fromRational $ toRational v]
compilePrimValue (BoolValue v) = Bool v
compilePrimValue Checked = Var "True"

compileExp :: Imp.Exp -> CompilerM op s CSExp

compileExp (Imp.ValueExp v) = return $ compilePrimValue v

compileExp (Imp.LeafExp (Imp.ScalarVar vname) _) =
  return $ Var $ compileName vname

compileExp (Imp.LeafExp (Imp.SizeOf t) _) =
  return $ simpleCall (compilePrimToCs $ IntType Int32) [Integer $ primByteSize t]

compileExp (Imp.LeafExp (Imp.Index src (Imp.Count iexp) bt DefaultSpace _) _) = do
  iexp' <- compileExp iexp
  let bt' = compilePrimType bt
  let indexFunction = compilePrimToCs bt
  return $ simpleCall indexFunction [Var $ compileName src, iexp']

compileExp (Imp.LeafExp (Imp.Index src (Imp.Count iexp) restype (Imp.Space space) _) _) =
  join $ asks envReadScalar
    <*> pure src <*> compileExp iexp
    <*> pure restype <*> pure space

compileExp (Imp.BinOpExp op x y) = do
  (x', y', simple) <- compileBinOpLike x y
  case op of
    Add{} -> simple "+"
    Sub{} -> simple "-"
    Mul{} -> simple "*"
    FAdd{} -> simple "+"
    FSub{} -> simple "-"
    FMul{} -> simple "*"
    FDiv{} -> simple "/"
    Xor{} -> simple "^"
    And{} -> simple "&"
    Or{} -> simple "|"
    Shl{} -> simple "<<"
    LogAnd{} -> simple "&&"
    LogOr{} -> simple "||"
    _ -> return $ simpleCall (pretty op) [x', y']

compileExp (Imp.ConvOpExp conv x) = do
  x' <- compileExp x
  return $ simpleCall (pretty conv) [x']

compileExp (Imp.CmpOpExp cmp x y) = do
  (x', y', simple) <- compileBinOpLike x y
  case cmp of
    CmpEq{} -> simple "=="
    FCmpLt{} -> simple "<"
    FCmpLe{} -> simple "<="
    _ -> return $ simpleCall (pretty cmp) [x', y']

compileExp (Imp.UnOpExp op exp1) = do
  exp1' <- compileExp exp1
  return $ UnOp (compileUnOp op) exp1'

compileExp (Imp.FunExp h args _) =
  simpleCall (futharkFun (pretty h)) <$> mapM compileExp args

compileCode :: Imp.Code op -> CompilerM op s ()

compileCode Imp.DebugPrint{} =
  return ()

compileCode (Imp.Op op) =
  join $ asks envOpCompiler <*> pure op

compileCode (Imp.If cond tb fb) = do
  cond' <- compileExp cond
  tb' <- collect $ compileCode tb
  fb' <- collect $ compileCode fb
  stm $ If cond' tb' fb'

compileCode (c1 Imp.:>>: c2) = do
  compileCode c1
  compileCode c2

compileCode (Imp.While cond body) = do
  cond' <- compileExp cond
  body' <- collect $ compileCode body
  stm $ While cond' body'

compileCode (Imp.For i it bound body) = do
  bound' <- compileExp bound
  let i' = compileName i
  body' <- collect $ compileCode body
  counter <- pretty <$> newVName "counter"
  one <- pretty <$> newVName "one"
  stm $ Assign (Var i') $ simpleCall (compilePrimToCs (IntType it)) [Integer 0]
  stm $ Assign (Var one) $ simpleCall (compilePrimToCs (IntType it)) [Integer 1]
  stm $ For counter (simpleCall "Enumerable.Range" [Integer 0, bound']) $
    body' ++ [AssignOp "+" (Var i') (Var one)]

compileCode (Imp.SetScalar vname exp1) = do
  let name' = Var $ compileName vname
  exp1' <- compileExp exp1
  stm $ Assign name' exp1'

compileCode Imp.DeclareMem{} = return ()
compileCode (Imp.DeclareScalar v Cert) =
  stm $ Assign (Var $ compileName v) $ Bool True
compileCode Imp.DeclareScalar{} = return ()

compileCode (Imp.DeclareArray name DefaultSpace t vs) = do
  atInit $ Assign name' $ CreateArray (Primitive $ compileType t) (map compilePrimValue vs)
  where name' = Var $ compileName name

compileCode (Imp.DeclareArray name (Space space) t vs) =
  join $ asks envStaticArray <*>
  pure name <*> pure space <*> pure t <*> pure vs

compileCode (Imp.Comment s code) = do
  code' <- collect $ compileCode code
  stm $ Comment s code'

compileCode (Imp.Assert e msg (loc,locs)) = do
  e' <- compileExp e
  stm $ Assert e' ("At " ++ stacktrace ++ ": " ++ msg)
  where stacktrace = intercalate " -> " (reverse $ map locStr $ loc:locs)

compileCode (Imp.Call dests fname args) = do
  args' <- mapM compileArg args
  let dests' = tupleOrSingle $ fmap Var (map compileName dests)
      fname' = futharkFun (pretty fname)
      call' = simpleCall fname' args'
  -- If the function returns nothing (is called only for side
  -- effects), take care not to assign to an empty tuple.
  stm $ if null dests
        then Exp call'
        else Assign dests' call'
  where compileArg (Imp.MemArg m) = return $ Var $ compileName m
        compileArg (Imp.ExpArg e) = compileExp e

compileCode (Imp.SetMem dest src _) = do
  let src' = Var (compileName src)
  let dest' = Var (compileName dest)
  stm $ Assign dest' src'

compileCode (Imp.Allocate name (Imp.Count e) DefaultSpace) = do
  e' <- compileExp e
  let allocate' = CreateArray (Primitive $ CSInt Int8T) [e']
  let name' = Var (compileName name)
  stm $ Assign name' allocate'

compileCode (Imp.Allocate name (Imp.Count e) (Imp.Space space)) =
  join $ asks envAllocate
    <*> pure name
    <*> compileExp e
    <*> pure space

compileCode (Imp.Copy dest (Imp.Count destoffset) DefaultSpace src (Imp.Count srcoffset) DefaultSpace (Imp.Count size)) = do
  destoffset' <- compileExp destoffset
  srcoffset' <- compileExp srcoffset
  let dest' = Var (compileName dest)
  let src' = Var (compileName src)
  size' <- compileExp size
  stm $ Exp $ simpleCall "Array.Copy" [src', srcoffset', dest', destoffset', size']

compileCode (Imp.Copy dest (Imp.Count destoffset) destspace src (Imp.Count srcoffset) srcspace (Imp.Count size)) = do
  copy <- asks envCopy
  join $ copy
    <$> pure dest <*> compileExp destoffset <*> pure destspace
    <*> pure src <*> compileExp srcoffset <*> pure srcspace
    <*> compileExp size <*> pure (IntType Int32) -- FIXME

compileCode (Imp.Write dest (Imp.Count idx) elemtype DefaultSpace _ elemexp) = do
  idx' <- compileExp idx
  elemexp' <- compileExp elemexp
  let dest' = Var $ compileName dest
  let elemtype' = compilePrimToCs elemtype
  let ctype = simpleCall elemtype' [elemexp']
  stm $ Exp $ simpleCall "writeScalarArray" [dest', idx', ctype]

compileCode (Imp.Write dest (Imp.Count idx) elemtype (Imp.Space space) _ elemexp) =
  join $ asks envWriteScalar
    <*> pure dest
    <*> compileExp idx
    <*> pure elemtype
    <*> pure space
    <*> compileExp elemexp

compileCode Imp.Skip = return ()

paramType :: Imp.Param -> PrimType
paramType (Imp.ScalarParam _ t) = t
paramType _ = Cert
