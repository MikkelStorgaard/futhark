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
                 , Exp $ getKernelCall kernel argnum t (Var tmp)]

        processKernelArg kernel argnum (Imp.MemKArg v) =
          return [ Exp $ getKernelCall kernel argnum IntPtrT (Var v) ]

        processKernelArg (Imp.SharedMemoryKArg (Imp.Count num_bytes)) =
          undefined
          {-
          num_bytes' <- Py.compileExp num_bytes
          return $ Py.simpleCall "cl.LocalMemory" [asLong num_bytes']
k-}

getKernelCall :: String -> Integer -> CSType -> CSExp -> CSExp
getKernelCall kernel arg_num t e =
  simpleCall "CL10.SetKernelArg" [ Var kernel, Integer arg_num
                                 , Cast IntPtrT $ sizeOf t, Ref e]

sizeOf :: CSType -> CSExp
sizeOf t = simpleCall "sizeOf" [Var $ ppr t]


asIntPtr :: CSExp -> CSExp
asIntPtr e = Cast e "IntPtr"

writeOpenCLScalar :: Py.WriteScalar Imp.OpenCL ()
writeOpenCLScalar mem i bt "device" val = do
  let mem' = Var $ Py.compileName mem
  let nparr = Call (Var "np.array")
              [Arg val, ArgKeyword "dtype" $ Var $ Py.compilePrimType bt]
  Py.stm $ Exp $ Call (Var "cl.enqueue_copy")
    [Arg $ Var "self.queue", Arg mem', Arg nparr,
     ArgKeyword "device_offset" $ asLong i,
     ArgKeyword "is_blocking" $ Var "synchronous"]

writeOpenCLScalar _ _ _ space _ =
  fail $ "Cannot write to '" ++ space ++ "' memory space."

readOpenCLScalar :: Py.ReadScalar Imp.OpenCL ()
readOpenCLScalar mem i bt "device" = do
  val <- newVName "read_res"
  let val' = Var $ pretty val
  let mem' = Var $ Py.compileName mem
  let nparr = Call (Var "np.empty")
              [Arg $ Integer 1,
               ArgKeyword "dtype" (Var $ Py.compilePrimType bt)]
  Py.stm $ Assign val' nparr
  Py.stm $ Exp $ Call (Var "cl.enqueue_copy")
    [Arg $ Var "self.queue", Arg val', Arg mem',
     ArgKeyword "device_offset" $ asLong i,
     ArgKeyword "is_blocking" $ Bool True]
  return $ Index val' $ IdxExp $ Integer 0

readOpenCLScalar _ _ _ space =
  fail $ "Cannot read from '" ++ space ++ "' memory space."

allocateOpenCLBuffer :: Py.Allocate Imp.OpenCL ()
allocateOpenCLBuffer mem size "device" =
  Py.stm $ Assign (Var $ Py.compileName mem) $
  Py.simpleCall "opencl_alloc" [Var "self", size, String $ pretty mem]

allocateOpenCLBuffer _ _ space =
  fail $ "Cannot allocate in '" ++ space ++ "' space"

copyOpenCLMemory :: Py.Copy Imp.OpenCL ()
copyOpenCLMemory destmem destidx Imp.DefaultSpace srcmem srcidx (Imp.Space "device") nbytes bt = do
  let srcmem'  = Var $ Py.compileName srcmem
  let destmem' = Var $ Py.compileName destmem
  let divide = BinOp "//" nbytes (Integer $ Imp.primByteSize bt)
  let end = BinOp "+" destidx divide
  let dest = Index destmem' (IdxRange destidx end)
  Py.stm $ ifNotZeroSize nbytes $
    Exp $ Call (Var "cl.enqueue_copy")
    [Arg $ Var "self.queue", Arg dest, Arg srcmem',
     ArgKeyword "device_offset" $ asLong srcidx,
     ArgKeyword "is_blocking" $ Var "synchronous"]

copyOpenCLMemory destmem destidx (Imp.Space "device") srcmem srcidx Imp.DefaultSpace nbytes bt = do
  let destmem' = Var $ Py.compileName destmem
  let srcmem'  = Var $ Py.compileName srcmem
  let divide = BinOp "//" nbytes (Integer $ Imp.primByteSize bt)
  let end = BinOp "+" srcidx divide
  let src = Index srcmem' (IdxRange srcidx end)
  Py.stm $ ifNotZeroSize nbytes $
    Exp $ Call (Var "cl.enqueue_copy")
    [Arg $ Var "self.queue", Arg destmem', Arg src,
     ArgKeyword "device_offset" $ asLong destidx,
     ArgKeyword "is_blocking" $ Var "synchronous"]

copyOpenCLMemory destmem destidx (Imp.Space "device") srcmem srcidx (Imp.Space "device") nbytes _ = do
  let destmem' = Var $ Py.compileName destmem
  let srcmem'  = Var $ Py.compileName srcmem
  Py.stm $ ifNotZeroSize nbytes $
    Exp $ Call (Var "cl.enqueue_copy")
    [Arg $ Var "self.queue", Arg destmem', Arg srcmem',
     ArgKeyword "dest_offset" $ asLong destidx,
     ArgKeyword "src_offset" $ asLong srcidx,
     ArgKeyword "byte_count" $ asLong nbytes]
  finishIfSynchronous

copyOpenCLMemory destmem destidx Imp.DefaultSpace srcmem srcidx Imp.DefaultSpace nbytes _ =
  Py.copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes

copyOpenCLMemory _ _ destspace _ _ srcspace _ _=
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace

staticOpenCLArray :: Py.StaticArray Imp.OpenCL ()
staticOpenCLArray name "device" t vs = do
  mapM_ Py.atInit <=< Py.collect $ do
    -- Create host-side Numpy array with intended values.
    Py.stm $ Assign (Var name') $
      Call (Var "np.array")
      [Arg $ List $ map Py.compilePrimValue vs,
       ArgKeyword "dtype" $ Var $ Py.compilePrimToNp t]

    -- Create memory block on the device.
    static_mem <- newVName "static_mem"
    let size = Integer $ genericLength vs * Imp.primByteSize t
    allocateOpenCLBuffer static_mem size "device"

    -- Copy Numpy array to the device memory block.
    Py.stm $ ifNotZeroSize size $
      Exp $ Call (Var "cl.enqueue_copy")
      [Arg $ Var "self.queue",
       Arg $ Var $ Py.compileName static_mem,
       Arg $ Call (Var "normaliseArray") [Arg (Var name')],
       ArgKeyword "is_blocking" $ Var "synchronous"]

    -- Store the memory block for later reference.
    Py.stm $ Assign (Field (Var "self") name') $
      Var $ Py.compileName static_mem

  Py.stm $ Assign (Var name') (Field (Var "self") name')
  where name' = Py.compileName name
staticOpenCLArray _ space _ _ =
  fail $ "PyOpenCL backend cannot create static array in memory space '" ++ space ++ "'"

packArrayOutput :: Py.EntryOutput Imp.OpenCL ()
packArrayOutput mem "device" bt ept dims =
  return $ Call (Var "cl.array.Array")
  [Arg $ Var "self.queue",
   Arg $ Tuple $ map Py.compileDim dims,
   Arg $ Var $ Py.compilePrimTypeExt bt ept,
   ArgKeyword "data" $ Var $ Py.compileName mem]
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
  Py.stm $ If (Var "synchronous") [Exp $ Py.simpleCall "self.queue.finish" []] []
