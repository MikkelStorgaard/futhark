{-# LANGUAGE TemplateHaskell #-}
module Futhark.CodeGen.Backends.GenericCSharp.Definitions
  ( csFunctions
  , csReader
  , csUtility
  , csPanic
  ) where

import Data.FileEmbed

csFunctions :: String
csFunctions = $(embedStringFile "rts/csharp/functions.cs")

csUtility :: String
csUtility = $(embedStringFile "rts/csharp/scalar.cs")

csReader :: String
csReader = $(embedStringFile "rts/csharp/reader.cs")

csPanic :: String
csPanic = $(embedStringFile "rts/csharp/panic.cs")
