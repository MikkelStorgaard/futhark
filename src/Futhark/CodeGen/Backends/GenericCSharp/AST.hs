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

data MemT = Pointer
          deriving (Eq, Show)

data ArgMemType = ArgOut
                | ArgRef
                deriving (Eq, Show)

instance Pretty ArgMemType where
  ppr ArgOut = text "out"
  ppr ArgRef = text "ref"

data CompType = ListT CSharpType
              | ArrayT CSharpType
              | TupleT [CSharpType]
              deriving (Eq, Show)

instance Pretty CompType where
  ppr (ArrayT t) = ppr t <> text "[]"
  ppr (TupleT ts) = text "Tuple" <> angles $ commasep $ map ppr ts

data CSharpPrim = CSharpInt CSharpIntT
                | CSharpFloat CSharpFloatT
                | CSharpBool Bool
                | CSharpString String
                | CSharpCustomT String
                -- dunno whether to fill on more of the necessary types yet,
                -- but that can be done eventually

data CSharpIntT = Int8
                | Int16
                | Int32
                | Int64
                deriving (Eq, Show)

data CSharpFloatT = Float
                  | Double
                  deriving (Eq, Show)

instance Pretty BaseT where
  ppr IntT = text "int"
  ppr DoubleT = text "double"
  ppr FloatT = text "float"
  ppr StringT = text "string"
  ppr (CustomT s) = text s

data CSharpType = CompositeType CompT
                | PointerType CSharpType
                | BaseType BaseT
                deriving (Eq, Show)

instance Pretty CSharpType where
  ppr (CompositeType t) = ppr t
  ppr (PointerType t) = parens(ppr t <> text "*")
  ppr (BaseType t) = ppr t

data PrivacyType = Blank
                 | Public
                 | Private
                 | Protected
                 | Internal
                 deriving (Eq, Show)

instance Pretty PrivacyType where
  ppr Blank = text "blank"
  ppr Public = text "public"
  ppr Private = text "private"
  ppr Protected = text "protected"
  ppr Internal = text "internal"

data Modifier = Const
              | Readonly
              | Static
              deriving (Eq, Show)

instance Pretty Modifier where
  ppr Const = text "const"
  ppr Readonly = text "readonly"
  ppr Static = text "static"

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
           | Ref String
           | Deref String
           | BinOp String CSharpExp CSharpExp
           | UnOp String CSharpExp
           | Cond CSharpExp CSharpExp CSharpExp
           | Index CSharpExp CSharpIdx
           | Call CSharpExp [CSharpArg]
           | Cast CSharpExp String
           | Tuple [CSharpExp]
           | Array [CSharpExp]
           | Field CSharpExp String
           | Dict [(CSharpExp, CSharpExp)]
           | Null
             deriving (Eq, Show)

instance Pretty CSharpExp where
  ppr (Integer x) = ppr x
  ppr (Bool x) = text $ (toLower . show) b
  ppr (Float x)
    | isInfinite x = text $ if x > 0 then "Double.PositiveInfinity" else "Double.NegativeInfinity"
    | otherwise = ppr x
  ppr (String x) = text $ show x
  ppr (RawStringLiteral s) = text "@\"" <> text s <> text "\""
  ppr (Var n) = text $ map (\x -> if x == '\'' then 'm' else x) n
  ppr (Ref n) =  text "&" <> text $ map (\x -> if x == '\'' then 'm' else x) n
  ppr (Deref n) =  text "*" <> text $ map (\x -> if x == '\'' then 'm' else x) n
  ppr (BinOp s e1 e2) = parens(ppr e1 <+> text s <+> ppr e2)
  ppr (UnOp s e) = text s <> parens (ppr e)
  ppr (Cond e1 e2 e3) = text "if" <+> parens(ppr e1) <> braces(ppr e2) <+> text "else" <> braces(ppr e3)
  ppr (Cast src bt) = parens(ppr bt) <+> ppr src
  ppr (Index src idx) = ppr src <> ppr idx

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
                | Assign PrivacyType [Modifier] (Maybe CSharpType) CSharpExp CSharpExp

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

type CSharpFunDefArg = (String, CSharpType)
data CSharpFunDef = Def String CSharpType [CSharpFunDefArg] [CSharpStmt]
                  deriving (Eq, Show)

data CSharpClassDef = Class PrivacyType [Modifier] [CSharpStmt]
                deriving (Eq, Show)
