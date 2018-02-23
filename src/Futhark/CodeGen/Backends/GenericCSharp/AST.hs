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

data MemType = Reference
             | Pointer
             deriving (Eq, Show)

data ArgMemType = ArgOut
                | ArgRef
                deriving (Eq, Show)

data CompType = ListT CSharpType
              | ArrayT CSharpType
              | TupleT [CSharpType]
              deriving (Eq, Show)

data DataType = CompType
              | IntT
              | DoubleT
              | FloatT
              | StringT
              | CustomT String
-- dunno whether to fill on more of the necessary types yet,
-- but that can be done eventually
              deriving (Eq, Show)

data PrivacyType = Blank
                 | Public
                 | Private
                 | Protected
                 | Internal
                 deriving (Eq, Show)

data Modifier = Const
              | Readonly
              | Static
              deriving (Eq, Show)

data CSharpType = CSharpType (Maybe MemType) DataType
                deriving (Eq, Show)


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
           | Null
             deriving (Eq, Show)

data CSharpIdx = IdxRange CSharpExp CSharpExp
               | IdxExp CSharpExp
               deriving (Eq, Show)


data CSharpArg = ArgKeyword String CSharpArg'
               | Arg CSharpArg'
               deriving (Eq, Show)

data CSharpArg' = Arg' (Maybe ArgMemType) (Maybe MemType) CSharpExp
                deriving (Eq, Show)


data CSharpStmt = If CSharpExp [CSharpStmt] [CSharpStmt]
                | Try [CSharpStmt] [CSharpExcept]
                | While CSharpExp [CSharpStmt]
                | For String CSharpExp [CSharpStmt]
                | With CSharpExp [CSharpStmt]
                | Unsafe [CSharpStmt]

                -- Maybe declare type (instead of just assigning a 'var'), and
                -- maybe cast assignment
                | Assign PrivacyType [Modifier] (Maybe CSharpType) CSharpExp
                                                (Maybe CSharpType) CSharpExp

                | AssignOp String CSharpExp CSharpExp
                | Comment String [CSharpStmt]
                | Assert CSharpExp String
                | Raise CSharpExp
                | Exp CSharpExp
                | Return CSharpExp
                | Pass

                -- Definition-like statements.
                | Import String (Maybe String)
                | FunDef CSharpFunDef
                | ClassDef CSharpClassDef

                -- Some arbitrary string of CSharp code.
                | Escape String
                deriving (Eq, Show)

data CSharpExcept = Catch CSharpExp [CSharpStmt]
              deriving (Eq, Show)

type CSharpFunDefArg = (Maybe ArgMemType, CSharpType, String)
data CSharpFunDef = Def String PrivacyType [Modifier] [CSharpFunDefArg] [CSharpStmt]
                  deriving (Eq, Show)

data CSharpClassDef = Class PrivacyType [Modifier] [CSharpStmt]
                deriving (Eq, Show)
