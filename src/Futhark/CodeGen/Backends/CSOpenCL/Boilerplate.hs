module Futhark.CodeGen.Backends.CSOpenCL.Boilerplate
  ( generateBoilerplate

  , kernelRuntime
  , kernelRuns
  ) where

import qualified Data.Map as M

import Futhark.CodeGen.ImpCode.OpenCL hiding (Index, If)
import Futhark.CodeGen.Backends.GenericCSharp as CS
import Futhark.CodeGen.Backends.GenericCSharp.AST as AST
import Futhark.CodeGen.OpenCL.Kernels


intT, stringT, intArrayT, stringArrayT :: CSType
intT = Primitive $ CSInt Int32T
stringT = Primitive StringT
intArrayT = Composite $ ArrayT intT
stringArrayT = Composite $ ArrayT stringT

generateBoilerplate :: String -> String -> [String] -> [PrimType]
                    -> M.Map VName SizeClass
                    -> CS.CompilerM OpenCL () ()
generateBoilerplate opencl_code opencl_prelude kernel_names types sizes = do
  final_inits <- CS.contextFinalInits

  let (opencl_fields, opencl_inits, top_decls, later_top_decls) =
        openClDecls kernel_names opencl_code opencl_prelude

  CS.atInit top_decls



  CS.stm $ AssignTyped stringArrayT (Var "size_names")
    (Just $ Collection "[]" (map (String . pretty) $ M.keys sizes))

  CS.stm $ AssignTyped stringArrayT (Var "size_classes")
    (Just $ Collection "[]" (map (String . pretty) $ M.elems sizes))


  let get_num_sizes = CS.publicName "get_num_sizes"
  let get_size_name = CS.publicName "get_size_name"
  let get_size_class = CS.publicName "get_size_class"


  CS.stm $ CS.funDef get_num_sizes intT [(intT, "i")]
    [ Return $ (Integer . toInteger) $ M.size sizes ]
  CS.stm $ CS.funDef get_size_name (Primitive StringT) [(intT, "i")]
    [ Return $ Index (Var "size_names") (IdxExp $ Var "i") ]

  CS.stm $ CS.funDef get_size_class (Primitive StringT) [(intT, "i")]
    [ Return $ Index (Var "size_classes") (IdxExp $ Var "i") ]

  let cfg = CS.publicName "context_config"
  let new_cfg = CS.publicName "context_config_new"
  let cfg_set_debugging = CS.publicName "context_config_set_debugging"
  let cfg_set_device = CS.publicName "context_config_set_device"
  let cfg_set_platform = CS.publicName "context_config_set_platform"
  let cfg_dump_program_to = CS.publicName "context_config_dump_program_to"
  let cfg_load_program_from = CS.publicName "context_config_load_program_from"
  let cfg_set_default_group_size = CS.publicName "context_config_set_default_group_size"
  let cfg_set_default_num_groups = CS.publicName "context_config_set_default_num_groups"
  let cfg_set_default_tile_size = CS.publicName "context_config_set_default_tile_size"
  let cfg_set_default_threshold = CS.publicName "context_config_set_default_threshold"
  let cfg_set_size = CS.publicName "context_config_set_size"

  CS.stm $ StructDef "sizes" (map (\k -> (intT, pretty k)) $ M.keys sizes)
  CS.stm $ StructDef cfg [ (CustomT "opencl_config", "opencl")
                         , (intArrayT, "sizes")]

  CS.stm $ CS.funDef new_cfg (CustomT cfg) []
    [ Assign (Var "cfg") $ CS.simpleInitClass cfg []
    , If (BinOp "==" (Var "cfg") Null) [Return Null][]
    , Reassign (Var "cfg.sizes") (Collection "int[]" (replicate (M.size sizes-1) (Integer 0)))
    , Exp $ CS.simpleCall "opencl_config_init" [ Out $ Var "cfg.opencl", (Integer . toInteger) $ M.size sizes
                                               , Var "size_names", Var "cfg.sizes", Var "size_classes" ]
    , Reassign (Var "cfg.opencl.transpose_block_dim") (Integer transposeBlockDim)
    , Return (Var cfg)
    ]

  CS.stm $ CS.funDef cfg_set_debugging VoidT [(OutT $ CustomT cfg, "cfg"),(Primitive BoolT, "flag")]
    [Reassign (Var "cfg.opencl.debugging") (Var "flag")]

  CS.stm $ CS.funDef cfg_set_device VoidT [(OutT $ CustomT cfg, "cfg"),(stringT, "s")]
    [Exp $ CS.simpleCall "set_preferred_device" [Out $ Var "cfg.opencl", Var "s"]]

  CS.stm $ CS.funDef cfg_set_platform VoidT [(OutT $ CustomT cfg, "cfg"),(stringT, "s")]
    [Exp $ CS.simpleCall "set_preferred_platform" [Out $ Var "cfg.opencl", Var "s"]]

  CS.stm $ CS.funDef cfg_dump_program_to VoidT [(OutT $ CustomT cfg, "cfg"),(stringT, "path")]
    [Reassign (Var "cfg.opencl.dump_program_to") (Var "path")]

  CS.stm $ CS.funDef cfg_load_program_from VoidT [(OutT $ CustomT cfg, "cfg"),(stringT, "path")]
    [Reassign (Var "cfg.opencl.load_program_from") (Var "path")]

  CS.stm $ CS.funDef cfg_set_default_group_size VoidT [(OutT $ CustomT cfg, "cfg"),(intT, "size")]
    [Reassign (Var "cfg.opencl.default_group_size") (Var "size")]

  CS.stm $ CS.funDef cfg_set_default_num_groups VoidT [(OutT $ CustomT cfg, "cfg"),(intT, "num")]
    [Reassign (Var "cfg.opencl.default_num_groups") (Var "num")]


  CS.stm $ CS.funDef cfg_set_default_tile_size VoidT [(OutT $ CustomT cfg, "cfg"),(intT, "size")]
    [Reassign (Var "cfg.opencl.default_tile_size") (Var "size")]

  CS.stm $ CS.funDef cfg_set_default_threshold VoidT [(OutT $ CustomT cfg, "cfg"),(intT, "size")]
    [Reassign (Var "cfg.opencl.default_threshold") (Var "size")]

  CS.stm $ CS.funDef cfg_set_size (Primitive BoolT) [(OutT $ CustomT cfg, "cfg")
                                                 ,(stringT, "size_name")
                                                 ,(intT, "size_value")]
    [ AST.For "i" ((Integer . toInteger) $ M.size sizes)
      [ If (BinOp "==" (Var "size_name") (Index (Var "size_names") (IdxExp (Var "i"))))
          [ Reassign (Index (Var "cfg.sizes") (IdxExp (Var "i"))) (Var "size_value")
          , Return (AST.Bool True)] []
      , Return $ AST.Bool False]]


--  (fields, init_fields) <- GC.contextContents

  mapM_ (\(tp, vname) -> atInit $ AssignTyped tp (Var vname) Nothing) $
    [(intT, "detail_memory")
    , (Primitive BoolT,  "debugging")
    , (CustomT "opencl_context", "opencl")
    , (CustomT "sizes", "sizes") ]
    ++ opencl_fields

  mapM_ CS.stm later_top_decls

  let set_required_types = [Reassign (Var "required_types") (Var "OPENCL_F64")
                           | FloatType Float64 `elem` types]

      set_sizes = zipWith (\i k -> Reassign (Field (Var "sizes") (show k))
                                            (Index (Var "cfg.sizes") (IdxExp $ (Integer . toInteger) i)))
                          [(0::Int)..] $ M.keys sizes

  let new_ctx = CS.publicName "context_new"
  let sync_ctx = CS.publicName "context_sync"

  CS.stm $ CS.funDef new_ctx VoidT [(CustomT cfg, "cfg")] $
    [ Reassign (Var "detail_memory") (Var "cfg.opencl.debugging")
    , Reassign (Var "debugging") (Var "cfg.opencl.debugging")
    , Reassign (Var "opencl.cfg") (Var "cfg.opencl")]
    ++ opencl_inits ++
    [ AssignTyped intT (Var "required_types") (Just $ Integer 0)
    , AssignTyped intT (Var "error") Nothing ]
    ++ set_required_types ++
    [ AssignTyped (CustomT "CLProgramHandle") (Var "prog")
        (Just $ CS.simpleCall "setup_opencl" [ Out $ Var "opencl"
                                             , Var "opencl_program"
                                             , Var "required_types"])]
    ++ concatMap loadKernelByName kernel_names
    ++ final_inits
    ++ set_sizes

  CS.stm $ CS.funDef sync_ctx intT []
    [ Exp $ CS.simpleCall "OPENCL_SUCCEED" [CS.simpleCall "CL10.Finish" [Var "queue"]]
    , Return $ Integer 0 ]

  mapM_ CS.debugReport $ openClReport kernel_names


openClDecls :: [String] -> String -> String
            -> ([(CSType, String)], [CSStmt], CSStmt, [CSStmt])
openClDecls kernel_names opencl_program opencl_prelude =
  (ctx_fields, ctx_inits, openCL_boilerplate, openCL_load)
  where ctx_fields =
          [ (intT, "total_runs")
          , (Primitive $ CSInt Int64T, "total_runtime")]
          ++ concatMap (\name -> [(CustomT "CLKernelHandle", name)
                                 ,(intT, kernelRuntime name)
                                 ,(intT, kernelRuns name)]) kernel_names

        ctx_inits =
          [ Reassign (Var "total_runs") (Integer 0)
          , Reassign (Var "total_runtime") (Integer 0) ]
          ++ concatMap (\name -> [ Reassign (Var $ kernelRuntime name) (Integer 0)
                                 , Reassign (Var $ kernelRuns name) (Integer 0)]
                  ) kernel_names

        openCL_load = [CS.funDef "post_opencl_setup" VoidT
            [(OutT $ CustomT "opencl_device_option", "option")] $ map sizeHeuristicsCode sizeHeuristicsTable]

        openCL_boilerplate =
          AssignTyped stringArrayT (Var "opencl_program")
              (Just $ Collection "string[]" [String $ opencl_prelude ++ opencl_program])

loadKernelByName :: String -> [CSStmt]
loadKernelByName name =
  [ Reassign (Var name)
      (CS.simpleCall "CL10.CreateKernel" [Var "prog", String name, Out $ Var "error"])
  , AST.Assert (BinOp "==" (Var "error") (Integer 0)) ""
  , If (Var "debugging")
      [Exp $ consoleErrorWriteLine "Created kernel {0}" [Var name]]
      []
  ]

kernelRuntime :: String -> String
kernelRuntime = (++"_total_runtime")

kernelRuns :: String -> String
kernelRuns = (++"_runs")

openClReport :: [String] -> [CSStmt]
openClReport names = report_kernels ++ [report_total]
  where longest_name = foldl max 0 $ map length names
        report_kernels = concatMap reportKernel names
        format_string name =
          let padding = replicate (longest_name - length name) ' '
          in unwords ["Kernel",
                      name ++ padding,
                      "executed {0} times, with average runtime: {1:0.000000}\tand total runtime: {2:0.000000}"]
        reportKernel name =
          let runs = kernelRuns name
              total_runtime = kernelRuntime name
          in [Exp $ consoleErrorWriteLine (format_string name)
               [ Var runs
               , Ternary (BinOp "!="
                           (BinOp "/"
                             (Cast (Primitive $ CSInt Int64T) (Var total_runtime))
                             (Var runs))
                           (Integer 0))
                 (Var runs) (Integer 1)
               , Cast (Primitive $ CSInt Int64T) $ Var total_runtime]
             , AssignOp "+" (Var "total_runtime") (Var total_runtime)
             , AssignOp "+" (Var "total_runs") (Var runs)
             ]

        ran_text = "Ran {0} kernels with cumulative runtime: {1:0.000000}"
        report_total = If (Var "debugging")
                          [Exp $ consoleErrorWriteLine ran_text [ Var "total_runs"
                                                                , Var "total_runtime"]
                          ] []

sizeHeuristicsCode :: SizeHeuristic -> CSStmt
sizeHeuristicsCode (SizeHeuristic platform_name device_type which what) =
  let which'' = BinOp "==" which' (Integer 0)
      option_contains_platform_name = CS.simpleCall "option.platform_name.Contains" [String platform_name]
      option_contains_device_type = BinOp "==" (Var "option_device_type") (Var $ clDeviceType device_type)
  in If (BinOp "&&" which''
          (BinOp "&&" option_contains_platform_name
                      option_contains_device_type))
          [ get_size ] []

  where clDeviceType DeviceGPU = "ComputeDeviceTypes.Gpu"
        clDeviceType DeviceCPU = "ComputeDeviceTypes.Cpu"

        which' = case which of
                   LockstepWidth -> Var "lockstep_width"
                   NumGroups ->     Var "cfg.default_num_groups"
                   GroupSize ->     Var "cfg.default_group_size"
                   TileSize ->      Var "cfg.default_tile_size"

        get_size = case what of
                     HeuristicConst x ->
                       Reassign which' (Integer $ toInteger x)

                     HeuristicDeviceInfo s ->
                       -- This only works for device info that fits in the variable.
                       let s' = "CL_DEVICE_" ++ s
                       in Exp $ CS.simpleCall "CL10.GetDeviceInfo"
                            [ Var "device", String s', CS.simpleCall "sizeOf" [which']
                            , Out which', Null ]
