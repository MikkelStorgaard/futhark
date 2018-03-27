{-# LANGUAGE FlexibleContexts #-}
module Futhark.CodeGen.Backends.PyOpenCL
  ( compileProg
  ) where

import Control.Monad
import Data.List

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


--maybe pass the config file rather than multiple arguments
compileProg :: MonadFreshNames m =>
               Maybe String -> Prog ExplicitMemory ->  m (Either InternalError String)
compileProg module_name prog = do
  res <- ImpGen.compileProg prog
  --could probably be a better why do to this..
  case res of
    Left err -> return $ Left err
    Right (Imp.Program opencl_code opencl_prelude kernel_names types sizes prog')  -> do
      --prepare the strings for assigning the kernels and set them as global
      let assign = unlines $ map (\x -> pretty $ Assign (Var ("self."++x++"_var")) (Var $ "program."++x)) Dt er kernel_names

      let defines =
            [Assign (Var "synchronous") $ Bool False,
             Assign (Var "preferred_platform") None,
             Assign (Var "preferred_device") None,
             Assign (Var "fut_opencl_src") $ RawStringLiteral $ opencl_prelude ++ opencl_code,
             Escape pyReader,
             Escape pyFunctions,
             Escape pyPanic]
      let imports = [Import "sys" Nothing,
                     Import "numpy" $ Just "np",
                     Import "ctypes" $ Just "ct",
                     Escape openClPrelude,
                     Import "pyopencl.array" Nothing,
                     Import "time" Nothing]

      let constructor = Py.Constructor [ "self"
                                       , "interactive=False"
                                       , "platform_pref=preferred_platform"
                                       , "device_pref=preferred_device"
                                       , "default_group_size=None"
                                       , "default_num_groups=None"
                                       , "default_tile_size=None"
                                       , "sizes={}"]
                        [Escape $ openClInit types assign sizes]
          options = [ Option { optionLongName = "platform"
                             , optionShortName = Just 'p'
                             , optionArgument = RequiredArgument
                             , optionAction =
                               [ Assign (Var "preferred_platform") $ Var "optarg" ]
                             }
                    , Option { optionLongName = "device"
                             , optionShortName = Just 'd'
                             , optionArgument = RequiredArgument
                             , optionAction =
                               [ Assign (Var "preferred_device") $ Var "optarg" ]
                             }]

      Right <$> Py.compileProg module_name constructor imports defines operations ()
        [Exp $ Py.simpleCall "self.queue.finish" []] options prog'
  where operations :: Py.Operations Imp.OpenCL ()
        operations = Py.Operations
                     { Py.opsCompiler = callKernel
                     , Py.opsWriteScalar = writeOpenCLScalar
                     , Py.opsReadScalar = readOpenCLScalar
                     , Py.opsAllocate = allocateOpenCLBuffer
                     , Py.opsCopy = copyOpenCLMemory
                     , Py.opsStaticArray = staticOpenCLArray
                     , Py.opsEntryOutput = packArrayOutput
                     , Py.opsEntryInput = unpackArrayInput
                     }

-- We have many casts to 'long', because PyOpenCL may get confused at
-- the 32-bit numbers that ImpCode uses for offsets and the like.
asLong :: CSExp -> CSExp
asLong x = CS.simpleCall "Convert.ToInt64" [x]

callKernel :: Py.OpCompiler Imp.OpenCL ()
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
  CS.stm $ Exp $ CS.simpleCall "CL10.EnqueueNDRangeKernel"
    [Var "queue", Var kernel_name', kernel_dims', workgroup_dims]
  finishIfSynchronous
  where processKernelArg :: String
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

getKernelCall :: String -> Integer -> CSExp -> CSExp -> CSExp
getKernelCall kernel arg_num size e =
  simpleCall "CL10.SetKernelArg" [ Var kernel, Integer arg_num, size, Ref e]

sizeOf :: CSType -> CSExp
sizeOf t = simpleCall "sizeOf" [Var $ ppr t]


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
  CS.stm $ Fixed assignArrayPointer (Var destmem') (Var ptr) $
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
      [ Var "queue", Arg srcmem', Arg destmem',
      , srcidx, destidx, nbytes
      , 0, Null, Null]
  finishIfSynchronous

copyOpenCLMemory destmem destidx Imp.DefaultSpace srcmem srcidx Imp.DefaultSpace nbytes _ =
  CS.copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes

copyOpenCLMemory _ _ destspace _ _ srcspace _ _=
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace

staticOpenCLArray :: CL.StaticArray Imp.OpenCL ()
staticOpenCLArray name "device" t vs = do
  mapM_ Py.atInit <=< Py.collect $ do
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
packArrayOutput mem "device" bt ept dims =
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
  let type_is_ok =
        BinOp "and"
        (BinOp "in" (Py.simpleCall "type" [e]) (List [Var "np.ndarray", Var "cl.array.Array"]))
        (BinOp "==" (Field e "dtype") (Var (Py.compilePrimToExtNp t s)))
  Py.stm $ Assert type_is_ok "Parameter has unexpected type"

  zipWithM_ (Py.unpackDim e) dims [0..]

  case memsize of
    Imp.VarSize sizevar ->
      Py.stm $ Assign (Var $ Py.compileName sizevar) $
      Py.simpleCall "np.int64" [Field e "nbytes"]
    Imp.ConstSize _ ->
      return ()

  let memsize' = Py.compileDim memsize
      pyOpenCLArrayCase =
        [Assign mem_dest $ Field e "data"]
  numpyArrayCase <- Py.collect $ do
    allocateOpenCLBuffer mem memsize' "device"
    Py.stm $ ifNotZeroSize memsize' $
      Exp $ Call (Var "cl.enqueue_copy")
      [Arg $ Var "self.queue",
       Arg $ Var $ Py.compileName mem,
       Arg $ Call (Var "normaliseArray") [Arg e],
       ArgKeyword "is_blocking" $ Var "synchronous"]

  Py.stm $ If (BinOp "==" (Py.simpleCall "type" [e]) (Var "cl.array.Array"))
    pyOpenCLArrayCase
    numpyArrayCase
  where mem_dest = Var $ Py.compileName mem
unpackArrayInput _ _ sid _ _ _ _ =
  fail $ "Cannot accept array from " ++ sid ++ " space."

ifNotZeroSize :: PyExp -> PyStmt -> PyStmt
ifNotZeroSize e s =
  If (BinOp "!=" e (Integer 0)) [s] []

finishIfSynchronous :: Py.CompilerM op s ()
finishIfSynchronous =
  CS.stm $ If (Var "synchronous") [Exp $ CS.simpleCall "CL10.Finish" [Var "queue"]] []
