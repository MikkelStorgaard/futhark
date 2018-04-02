{-# LANGUAGE FlexibleContexts #-}
module Futhark.CodeGen.Backends.CSOpenCL
  ( compileProg
  ) where

import Control.Monad
import Data.List

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.OpenCL as C

import Futhark.Error
import Futhark.Representation.ExplicitMemory (Prog, ExplicitMemory)
import Futhark.CodeGen.Backends.CSOpenCL.Boilerplate
import qualified Futhark.CodeGen.Backends.GenericCSharp as CS
import qualified Futhark.CodeGen.ImpCode.OpenCL as Imp
import qualified Futhark.CodeGen.ImpGen.OpenCL as ImpGen
import Futhark.CodeGen.Backends.GenericCSharp.AST
import Futhark.CodeGen.Backends.GenericCSharp.Options
import Futhark.CodeGen.Backends.GenericCSharp.Definitions
import Futhark.Util.Pretty(pretty)
import Futhark.MonadFreshNames


compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m (Either InternalError String)
compileProg prog = do
  res <- ImpGen.compileProg prog
  case res of
    Left err -> return $ Left err
    Right (Program opencl_code opencl_prelude kernel_names types sizes prog') ->
      Right <$> CS.compileProg operations
                (generateBoilerplate opencl_code opencl_prelude kernel_names types sizes) ()
                [Space "device", Space "local", DefaultSpace]
                cliOptions prog'
  where operations :: CS.Operations OpenCL ()
        operations = CS.Operations
                     { CS.opsCompiler = callKernel
                     , CS.opsWriteScalar = writeOpenCLScalar
                     , CS.opsReadScalar = readOpenCLScalar
                     , CS.opsAllocate = allocateOpenCLBuffer
                     , CS.opsDeallocate = deallocateOpenCLBuffer
                     , CS.opsCopy = copyOpenCLMemory
                     , CS.opsStaticArray = staticOpenCLArray
                     , CS.opsMemoryType = openclMemoryType
                     , CS.opsFatMemory = True
                     }

cliOptions :: [Option]
cliOptions = [ Option { optionLongName = "platform"
                      , optionShortName = Just "p"
                      , optionArgument = RequiredArgument
                      , optionAction = [Escape "futhark_context_config_set_platform(cfg, optarg);"]
                      }
             , Option { optionLongName = "device"
                      , optionShortName = Just 'd'
                      , optionArgument = RequiredArgument
                      , optionAction = [Escape "futhark_context_config_set_device(cfg, optarg);"]
                      }
             ]

{-
             , Option { optionLongName = "default-group-size"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument
                      , optionAction = [C.cstm|futhark_context_config_set_default_group_size(cfg, atoi(optarg));|]
                      }
             , Option { optionLongName = "default-num-groups"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument
                      , optionAction = [C.cstm|futhark_context_config_set_default_num_groups(cfg, atoi(optarg));|]
                      }
             , Option { optionLongName = "default-tile-size"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument
                      , optionAction = [C.cstm|futhark_context_config_set_default_tile_size(cfg, atoi(optarg));|]
                      }
             , Option { optionLongName = "default-threshold"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument
                      , optionAction = [C.cstm|futhark_context_config_set_default_threshold(cfg, atoi(optarg));|]
                      }
             , Option { optionLongName = "dump-opencl"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument
                      , optionAction = [C.cstm|futhark_context_config_dump_program_to(cfg, optarg);|]
                      }
             , Option { optionLongName = "load-opencl"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument
                      , optionAction = [C.cstm|futhark_context_config_load_program_from(cfg, optarg);|]
                      }
             , Option { optionLongName = "print-sizes"
                      , optionShortName = Nothing
                      , optionArgument = NoArgument
                      , optionAction = [C.cstm|{
                          int n = futhark_get_num_sizes();
                          for (int i = 0; i < n; i++) {
                            printf("%s (%s)\n", futhark_get_size_name(i),
                                                futhark_get_size_class(i));
                          }
                          exit(0);
                        }|]
                      }
             , Option { optionLongName = "size"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument
                      , optionAction = [C.cstm|{
                          char *name = optarg;
                          char *equals = strstr(optarg, "=");
                          char *value_str = equals != NULL ? equals+1 : optarg;
                          int value = atoi(value_str);
                          if (equals != NULL) {
                            *equals = 0;
                            if (futhark_context_config_set_size(cfg, name, value) != 0) {
                              panic(1, "Unknown size: %s\n", name);
                            }
                          } else {
                            panic(1, "Invalid argument for size option: %s\n", optarg);
                          }}|]
                      }
             ]
-}


callKernel :: CS.OpCompiler Imp.OpenCL ()
callKernel (Imp.GetSize v key) =
  CS.stm $ Assign (Var (CS.compileName v)) $
  Index (Var "sizes") (IdxExp $ String $ pretty key)
callKernel (Imp.GetSizeMax v size_class) =
  CS.stm $ Assign (Var (CS.compileName v)) $
  Var $ "max_" ++ pretty size_class
callKernel (Imp.HostCode c) =
  CS.compileCode c

callKernel (Imp.LaunchKernel name args kernel_size workgroup_size) = do
  kernel_size' <- mapM CS.compileExp kernel_size
  let total_elements = foldl mult_exp (Integer 1) kernel_size'
  let cond = BinOp "!=" total_elements (Integer 0)
  workgroup_size' <- Tuple <$> mapM (fmap CS.compileExp) workgroup_size
  body <- Py.collect $ launchKernel name kernel_size' workgroup_size' args
  Py.stm $ If cond body []
  where mult_exp = BinOp "*"

launchKernel :: String -> [CSExp] -> CSExp -> [Imp.KernelArg] -> CS.CompilerM op s ()
launchKernel kernel_name kernel_dims workgroup_dims args = do
  let kernel_dims' = CreateArray (CS.compilePrimTypeToAST Int32) kernel_dims
  let kernel_name' = kernel_name ++ "_var"
  args_stms <- mapM (processKernelArg kernel_name') (zip [0..] args)

  CS.stm $ Unsafe $ concat arg_stms

  global_work_size <- newVName "global_work_size"
  local_work_size <- newVName "local_work_size"
  stop_watch <- newVName "stop_watch"
  time_diff <- newVName "time_diff"

  CS.stm $ If (BinOp "!=" total_elements (Integer 0))
    [ Assign (Var global_work_size) (Collection "int[]" kernel_dims)
    , Assign (Var local_work_size) (Collection "int[]" workgroup_dims)
    , Assign (Var stop_watch) $ simpleInitClass "Stopwatch" []
    , If (Var "debugging") [debugStartStmts] []
    ]
    ++
    [ Exp $ CS.simpleCall "OPENCL_SUCCEED" [
        CS.simpleCall "CL10.EnqueueNDRangeKernel"
          [Var "queue", Var kernel_name', kernel_dims', workgroup_dims]]]
    ++
    [ If (Var "debugging") debugEndStmts [] ]
  finishIfSynchronous

  where debugStartStmts =
          map Exp [ consoleErrorWrite "Launching {0} with global work size [" [String kernel_name]
                  , printKernelSize global_work_size
                  , consoleErrorWrite "] and local work size [" []
                  , printKernelSize local_work_size
                  , consoleErrorWrite "].\n" []
                  , callMethod (Var stop_watch) "Start" []
                  ]
        debugEndStmts =
          [ Exp $ CS.simpleCall "OPENCL_SUCCEED" [
              CS.simpleCall "CL10.Finish"
                [Var "queue"]]
          , Exp $ callMethod (Var stop_watch) "Stop" []
          , Assign (Var time_diff) (Field (Var stop_watch) "ElapsedMilliseconds")
          , AssignOp "+" (Var kernelRuntime kernel_name) (Field (Var stop_watch) "ElapsedMilliseconds")
          , AssignOp "+" (Var kernelRuns kernel_name) (Integer 1)
          , Exp $ consoleErrorWriteLine "kernel {0} runtime: {1}" [String kernel_name, Var time_diff]
          ]

        processKernelArg :: String
                         -> Imp.KernelArg
                         -> Integer
                         -> CS.CompilerM op s [CSStmt]
        processKernelArg kernel argnum (Imp.ValueKArg e bt) = do
          let t = compilePrimTypeToAST bt
          tmp <- newVName "kernel_arg"
          e' <- Py.compileExp e
          return [ AssignTyped t (Var tmp) (Just e')
                 , Exp $ getKernelCall kernel argnum (sizeOf t) (Var tmp)]

        processKernelArg kernel argnum (Imp.MemKArg v) =
          return [ Exp $ getKernelCall kernel argnum (sizeOf IntPtrT) (Var v) ]

        processKernelArg (Imp.SharedMemoryKArg (Imp.Count num_bytes)) = do
          num_bytes' <- CS.compileExp num_bytes
          return [ Exp $ getKernelCall kernel argnum num_bytes' Null ]

        kernel_rank = length kernel_dims
        total_elements = foldl (BinOp "*") (Integer 1) kernel_dims

        printKernelSize :: VName -> [C.Stm]
        printKernelSize work_size =
          intercalate [consoleErrorWrite ", " []]
          map (printKernelDim work_size) [0..kernel_rank-1]

        printKernelDim global_work_size i =
          consoleErrorWrite "{0}" [Index (Var global_work_size) (IdxExp (Integer i))]



getKernelCall :: String -> Integer -> CSExp -> CSExp -> CSExp
getKernelCall kernel arg_num size e =
  simpleCall "CL10.SetKernelArg" [ Var kernel, Integer arg_num, size, Ref e]

castToIntPtr :: CSExp -> CSExp
castToIntPtr e = Cast e "IntPtr"

toIntPtr :: CSExp -> CSExp
toIntPtr e = CS.simpleInitClass "IntPtr" [e]

writeOpenCLScalar :: CS.WriteScalar Imp.OpenCL ()
writeOpenCLScalar mem i bt "device" val = do
  let mem' = Var $ CS.compileName mem
  let bt' = CS.compilePrimTypeToAST bt
  scalar <- newVName "scalar"
  ptr <- newVName "ptr"
  CS.stm $ AssignTyped bt' (Var scalar) (Just val)
  CS.stm $ AssignTyped (PointerT VoidT) (Var ptr) (Just $ Ref scalar)
  simpleCall "CL10.EnqueueWriteBuffer"
    [ Var "queue", Var mem', Var "synchronous"
    , toIntPtr i, toIntPtr $ sizeOf bt', toIntPtr $ Var ptr
    , Integer 0, Null, Null]

writeOpenCLScalar _ _ _ space _ =
  fail $ "Cannot write to '" ++ space ++ "' memory space."

readOpenCLScalar :: CS.ReadScalar Imp.OpenCL ()
readOpenCLScalar mem i bt "device" = do
  val <- newVName "read_res"
  let bt' = CS.compilePrimTypeToAST bt
  let mem' = Var $ CS.compileName mem
  CS.stm $ AssignTyped bt' (Var val) (Just simpleInitClass (ppr bt') [])
  CS.stm $ simpleCall "CL10.EnqueueReadBuffer"
             [ Var "queue", Var mem' , Bool True
             , toIntPtr i, sizeOf bt', toIntPtr $ Ref $ Var val
             , Integer 0, Null, Null]
  return $ Var val

readOpenCLScalar _ _ _ space =
  fail $ "Cannot read from '" ++ space ++ "' memory space."

allocateOpenCLBuffer :: CS.Allocate Imp.OpenCL ()
allocateOpenCLBuffer mem size "device" = do
  CS.stm $ Assign (Var $ CS.compileName mem) $
    simpleCall "CL10.CreateBuffer" [ Var "context", Var "ComputeMemoryFlags.ReadWrite"
                                   , toIntPtr size, Var "IntPtr.Zero"
                                   , Out $ Var "compute_err_code"  ]

allocateOpenCLBuffer _ _ space =
  fail $ "Cannot allocate in '" ++ space ++ "' space"

copyOpenCLMemory :: CS.Copy Imp.OpenCL ()
copyOpenCLMemory destmem destidx Imp.DefaultSpace srcmem srcidx (Imp.Space "device") nbytes bt = do
  let srcmem'  = Var $ Py.compileName srcmem
  let destmem' = Var $ Py.compileName destmem
  ptr <- newVName "ptr"
  CS.stm $ Fixed (assignArrayPointer (Var destmem') (Var ptr)) $
    [ ifNotZeroSize nbytes $
      Exp $ simpleCall "CL10.EnqueueReadBuffer"
      [ Var "queue", Var srcmem', Var "synchronous"
      , toIntPtr i, nbytes, toIntPtr $ Var ptr
      , destidx, Null, Null]
    ]

copyOpenCLMemory destmem destidx (Imp.Space "device") srcmem srcidx Imp.DefaultSpace nbytes _ = do
  let destmem' = Var $ Py.compileName destmem
  let srcmem'  = Var $ Py.compileName srcmem
  ptr <- newVName "ptr"
  CS.stm $ Fixed assignArrayPointer (Var srcmem') (Var ptr) $
    [ ifNotZeroSize nbytes $
      Exp $ simpleCall "CL10.EnqueueWriteBuffer"
        [ Var "queue", Var destmem', Var "synchronous"
        , toIntPtr destidx, nbytes, toIntPtr $ Var ptr
        , srcidx, Null, Null]
    ]

copyOpenCLMemory destmem destidx (Imp.Space "device") srcmem srcidx (Imp.Space "device") nbytes _ = do
  let destmem' = Var $ Py.compileName destmem
  let srcmem'  = Var $ Py.compileName srcmem
  CS.stm $ ifNotZeroSize nbytes $
    Exp $ simpleCall "CL10.EnqueueCopyBuffer"
      [ Var "queue", Arg srcmem', Arg destmem'
      , srcidx, destidx, nbytes
      , 0, Null, Null]
  finishIfSynchronous

copyOpenCLMemory destmem destidx Imp.DefaultSpace srcmem srcidx Imp.DefaultSpace nbytes _ =
  CS.copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes

copyOpenCLMemory _ _ destspace _ _ srcspace _ _=
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace

staticOpenCLArray :: CL.StaticArray Imp.OpenCL ()
staticOpenCLArray name "device" t vs = do
  mapM_ CS.atInit <=< CS.collect $ do
    -- Create host-side C# array with intended values.
    tmp_arr <- newVName "tmp_arr"
    Py.stm $ Assign (Var tmp_arr) $
      CreateArray (CS.compilePrimTypeToAST t) [map CS.compilePrimValue vs]

    -- Create memory block on the device.
    static_mem <- newVName "static_mem"
    ptr <- newVName "ptr"
    let size = Integer $ genericLength vs * Imp.primByteSize t
    allocateOpenCLBuffer static_mem size "device"

    -- Copy Numpy array to the device memory block.
    CS.stm $ Fixed (assignArrayPointer tmp_arr ptr) $
      [ ifNotZeroSize size $
          Exp $ simpleCall "CL10.EnqueueWriteBuffer" $
          [ Var "queue", Var $ CS.compileName static_mem, Var "synchronous"
          , toIntPtr (Integer 0), toIntPtr size
          , Var ptr, Integer 0, Null, Null ]

                                                  ]
    -- Store the memory block for later reference.
    CS.stm $ Assign name' $ Var $ CS.compileName static_mem

  CS.stm $ Reassign (Var name') (Var name')
  where name' = CS.compileName name
staticOpenCLArray _ space _ _ =
  fail $ "PyOpenCL backend cannot create static array in memory space '" ++ space ++ "'"

assignArrayPointer :: CSExp -> CSExp -> CSStmt
assignArrayPointer e ptr =
  AssignTyped (PointerT VoidT) (Var ptr) (Just $ Ref $ Index e (IdxExp $ Integer 0))

assignScalarPointer :: CSExp -> CSExp -> CSStmt
assignScalarPointer e ptr =
  AssignTyped (PointerT VoidT) (Var ptr) (Just $ Ref e)

packArrayOutput :: Py.EntryOutput Imp.OpenCL ()
packArrayOutput mem "device" bt ept dims = do
  tmp_arr <- newVName "tmp_arr"
  ptr <- newVName "ptr"
  let size = foldr (BinOp "*") (Integer 1) dims
  let bt' = CS.compilePrimTypeExt bt ept
  let nbytes = BinOp "*" (sizeOf bt') size

  CS.stm $ Assign (Var tmp_arr) $ AllocArray bt' size
  CS.stm $ Fixed (assignArrayPointer (Var tmp_arr) (Var ptr)) $
    [ Exp $ simpleCall "CL10.EnqueueReadBuffer"
      [ Var "queue", Var mem, Var "synchronous"
      , toIntPtr $ Integer 0, toIntPtr nbytes, toIntPtr $ Var ptr
      , toIntPtr $ Integer 0, Null, Null]
    ]
  return $ CS.parametrizedCall "new FlatArray" bt' [Var tmp_arr, dims]

packArrayOutput _ sid _ _ _ =
  fail $ "Cannot return array from " ++ sid ++ " space."

unpackArrayInput :: Py.EntryInput Imp.OpenCL ()
unpackArrayInput mem memsize "device" t s dims e = do
  let size = foldr (BinOp "*") (Integer 1) dims
  let t' = CS.compilePrimTypeExt t s
  let nbytes = BinOp "*" (sizeOf t') size
  zipWithM_ (CS.unpackDim e) dims [0..]
  ptr <- newVName "ptr"

  let memsize' = CS.compileDim memsize
  numpyArrayCase <- CS.collect $ do
    allocateOpenCLBuffer mem memsize' "device"
    CS.stm $ Fixed assignArrayPointer (Ref $ Index (Field (Var e) "array") (IdxExp $ Integer 0)) (Var ptr) $
      [ CS.stm $ ifNotZeroSize memsize' $
        CS.simpleCall "CL10.EnqueueWriteBuffer"
        [ Var "queue", Var $ CS.compileName mem, Var "synchronous"
        , toIntPtr (Integer 0), memsize', toIntPtr (Var ptr)
        , toIntPtr (Integer 0), Null, Null]
      ]

  CS.stm numpyArrayCase
  where mem_dest = Var $ Py.compileName mem
unpackArrayInput _ _ sid _ _ _ _ =
  fail $ "Cannot accept array from " ++ sid ++ " space."

ifNotZeroSize :: PyExp -> PyStmt -> PyStmt
ifNotZeroSize e s =
  If (BinOp "!=" e (Integer 0)) [s] []

finishIfSynchronous :: Py.CompilerM op s ()
finishIfSynchronous =
  CS.stm $ If (Var "synchronous") [Exp $ CS.simpleCall "CL10.Finish" [Var "queue"]] []
