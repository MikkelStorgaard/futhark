module Futhark.CodeGen.Backends.GenericCSharp.AST
  ( CSharpExp(..)
  , CSharpIdx (..)
  , CSharpArg (..)
  , CSharpStmt(..)
  , module Language.Futhark.Core
  , CSharpProg(..)
  , CSharpExcept(..)
  , CSharpFunDef(..)
  , CSharpClassDef(..)
  )
  where

import Language.Futhark.Core
import Futhark.Util.Pretty


data UnOp = Not -- ^ Boolean negation.
          | Complement -- ^ Bitwise complement.
          | Negate -- ^ Numerical negation.
          | Abs -- ^ Absolute/numerical value.
            deriving (Eq, Show)

data CSharpExp = Integer Integer
           | Bool Bool
           | Float Double
           | String String
           | RawStringLiteral String
           | Var String
           | BinOp String CSharpExp CSharpExp
           | UnOp String CSharpExp
           | Cond CSharpExp CSharpExp CSharpExp
           | Index CSharpExp CSharpIdx
           | Call CSharpExp [CSharpArg]
           | Cast CSharpExp String
           | Tuple [CSharpExp]
           | List [CSharpExp]
           | Field CSharpExp String
           | Dict [(CSharpExp, CSharpExp)]
           | None
             deriving (Eq, Show)
