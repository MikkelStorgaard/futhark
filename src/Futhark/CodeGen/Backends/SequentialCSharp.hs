module Futhark.CodeGen.Backends.SequentialCSharp
     ( compileProg
     ) where

import Control.Monad

import Futhark.Error
import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.ImpCode.Sequential as Imp
import qualified Futhark.CodeGen.ImpGen.Sequential as ImpGen
import qualified Futhark.CodeGen.Backends.GenericCSharp as GenericCSharp
import Futhark.CodeGen.Backends.GenericCSharp.AST (CSStmt(Using))

import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m =>
               Maybe String -> Prog ExplicitMemory -> m (Either InternalError String)
compileProg module_name =
  ImpGen.compileProg >=>
  traverse (GenericCSharp.compileProg
            module_name
            GenericCSharp.emptyConstructor
            imports
            defines
            operations () [] [])
  where imports = [ Using Nothing False "System"
                  , Using Nothing False "System.Diagnostics"
                  , Using Nothing False "System.ValueTuple"
                  , Using Nothing True "System.Convert"
                  , Using Nothing True "System.Math"
                  ]
        defines = [Escape csScalar, Escape csPanic]
        operations :: GenericCSharp.Operations Imp.Sequential ()
        operations = GenericCSharp.defaultOperations
                     { GenericCSharp.opsCompiler = const $ return ()
                     , GenericCSharp.opsCopy = copySequentialMemory
                     }

copySequentialMemory :: GenericCSharp.Copy Imp.Sequential ()
copySequentialMemory destmem destidx DefaultSpace srcmem srcidx DefaultSpace nbytes _bt =
  GenericCSharp.copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes
copySequentialMemory _ _ destspace _ _ srcspace _ _ =
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace
