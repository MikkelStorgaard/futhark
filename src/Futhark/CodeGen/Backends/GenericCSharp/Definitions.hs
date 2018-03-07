{-# LANGUAGE TemplateHaskell #-}
module Futhark.CodeGen.Backends.GenericCSharp.Definitions
  ( csFunctions
  , csReader
  , csScalar
  , csPanic
  ) where

import Data.FileEmbed

csFunctions :: String
csFunctions = $(embedStringFile "rts/csharp/functions.cs")

csScalar :: String
csScalar = $(embedStringFile "rts/csharp/scalar.cs")

csReader :: String
csReader = $(embedStringFile "rts/csharp/reader.cs")

csPanic :: String
csPanic = $(embedStringFile "rts/csharp/panic.cs")
