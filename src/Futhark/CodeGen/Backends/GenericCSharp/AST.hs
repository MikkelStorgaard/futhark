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
import Data.Text (toLower)

data MemT = Pointer
          deriving (Eq, Show)

data ArgMemType = ArgOut
                | ArgRef
                deriving (Eq, Show)

instance Pretty ArgMemType where
  ppr ArgOut = text "out"
  ppr ArgRef = text "ref"

instance Pretty CompType where
  ppr (ArrayT t) = ppr t <> text "[]"
  ppr (TupleT ts) = undefined

data IntType = Int8T
             | Int16T
             | Int32T
             | Int64T
             deriving (Eq, Show)

data FloatType = FloatT
               | DoubleT
               deriving (Eq, Show)

data CSharpType = Composite CompType
                | PointerT CSharpType
                | Primitive PrimType
                deriving (Eq, Show)

data CompType = ArrayT CSharpType
              | TupleT [CSharpType]
              deriving (Eq, Show)

data PrimType = IntType IntType
              | FloatType FloatType
              | BoolT
              | ByteT
              deriving (Eq, Show)



instance Pretty CSharpType where
  ppr (Composite t) = ppr t
  ppr (PointerT t) = parens(ppr t <> text "*")
  ppr (Primitive t) = ppr t

instance Pretty PrimType where
  ppr BoolT = text "bool"
  ppr ByteT = text "byte"
  ppr (IntType t) = ppr t
  ppr (FloatType t) = ppr t

instance Pretty IntType where
  ppr Int8T = text "byte"
  ppr Int16T = text "short"
  ppr Int32T = text "int"
  ppr Int64T = text "long"

instance Pretty FloatType where
  ppr FloatT = text "float"
  ppr DoubleT = text "double"

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
  ppr (Float x)
    | isInfinite x = text $ if x > 0 then "Double.PositiveInfinity" else "Double.NegativeInfinity"
    | otherwise = ppr x
  ppr (Bool True) = text "true"
  ppr (Bool False) = text "false"
  ppr (String x) = text $ show x
  ppr (RawStringLiteral s) = text "@\"" <> text s <> text "\""
  ppr (Var n) = text $ map (\x -> if x == '\'' then 'm' else x) n
  ppr (Ref n) =  text "&" <> text (map (\x -> if x == '\'' then 'm' else x) n)
  ppr (Deref n) =  text "*" <> text (map (\x -> if x == '\'' then 'm' else x) n)
  ppr (BinOp s e1 e2) = parens(ppr e1 <+> text s <+> ppr e2)
  ppr (UnOp s e) = text s <> parens (ppr e)
  ppr (Cond e1 e2 e3) = text "if" <+> parens(ppr e1) <> braces(ppr e2) <+> text "else" <> braces(ppr e3)
  ppr (Cast src bt) = parens(ppr bt) <+> ppr src
  ppr (Index src (IdxExp idx)) = ppr src <> brackets(ppr idx)
  ppr (Index src (IdxRange from to)) = text "MySlice" <> parens(commasep $ map ppr [src, from, to])

data CSharpIdx = IdxRange CSharpExp CSharpExp
               | IdxExp CSharpExp
               deriving (Eq, Show)

data CSharpArg = ArgKeyword CSharpArg -- please don't assign multiple keywords with the same argument
               | Arg (Maybe ArgMemType) CSharpExp
               deriving (Eq, Show)

data CSharpStmt = If CSharpExp [CSharpStmt] [CSharpStmt]
                | Try [CSharpStmt] [CSharpExcept]
                | While CSharpExp [CSharpStmt]
                | For String CSharpExp [CSharpStmt]
                | With CSharpExp [CSharpStmt]
                | Unsafe [CSharpStmt]
                -- Maybe declare type (instead of just assigning a 'var'), and
                | Assign CSharpExp CSharpExp
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

instance Pretty CSharpStmt where
  ppr (If cond tbranch fbranch) =
    text "if" <+> parens(ppr cond) </>
    lbrace </>
    indent 4 (stack $ map ppr tbranch) </>
    rbrace </>
    text "else"
    lbrace </>
    indent 4 (stack $ map ppr fbranch) </>
    rbrace

  ppr (Try stmts excepts) =
    text "try" </>
    lbrace </>
    indent 4 (stack $ map ppr stmts) </>
    rbrace </>
    ppr excepts

  ppr (While cond body) =
    text "while" <+> parens(ppr cond) </>
    lbrace </>
    indent 4 (stack $ map ppr stmts) </>
    rbrace

  ppr (For i what body) =
    text "foreach" <> parens(text "var" <+> text i <+> text "in" <+> ppr what) </>
    lbrace </>
    indent 4 (stack $ map ppr stmts) </>
    rbrace

  ppr (ClassDef d) = ppr d

  ppr (FunDef d) = ppr d

data CSharpExcept = Catch CSharpExp [CSharpStmt]
              deriving (Eq, Show)

type CSharpFunDefArg = (String, CSharpType)
data CSharpFunDef = Def String CSharpType [CSharpFunDefArg] [CSharpStmt]
                  deriving (Eq, Show)

data CSharpClassDef = Class String [CSharpStmt]
                deriving (Eq, Show)

newtype CSharpProg = CSharpProg [CSharpStmt]
                   deriving (Eq, Show)

instance Pretty CSharpProg where
  ppr (CSharpProg stms) = stack (map ppr stms)
