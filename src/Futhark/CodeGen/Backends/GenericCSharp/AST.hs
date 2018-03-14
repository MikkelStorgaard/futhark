{-# LANGUAGE PostfixOperators #-}


module Futhark.CodeGen.Backends.GenericCSharp.AST
  ( CSExp(..)
  , CSType(..)
  , CSComp(..)
  , CSPrim(..)
  , CSInt(..)
  , CSFloat(..)
  , CSIdx (..)
  , CSArg (..)
  , CSStmt(..)
  , module Language.Futhark.Core
  , CSProg(..)
  , CSExcept(..)
  , CSFunDef(..)
  , CSFunDefArg
  , CSClassDef(..)
  , CSConstructorDef(..)
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

instance Pretty CSComp where
  ppr (ArrayT t) = ppr t <> text "[]"
  ppr (TupleT ts) = parens(commasep $ map ppr ts)

data CSInt = Int8T
           | Int16T
           | Int32T
           | Int64T
           deriving (Eq, Show)

data CSFloat = FloatT
             | DoubleT
             deriving (Eq, Show)

data CSType = Composite CSComp
            | PointerT CSType
            | Primitive CSPrim
            | MemoryT String
            | VoidT
            | CustomT String
            deriving (Eq, Show)

data CSComp = ArrayT CSType
            | TupleT [CSType]
            deriving (Eq, Show)

data CSPrim = CSInt CSInt
            | CSFloat CSFloat
            | BoolT
            | ByteT
            deriving (Eq, Show)

instance Pretty CSType where
  ppr (Composite t) = ppr t
  ppr (PointerT t) = parens(ppr t <> text "*")
  ppr (Primitive t) = ppr t
  ppr (CustomT t) = text t
  ppr VoidT = text "void"
  ppr (MemoryT _) = text "byte[]"

instance Pretty CSPrim where
  ppr BoolT = text "bool"
  ppr ByteT = text "byte"
  ppr (CSInt t) = ppr t
  ppr (CSFloat t) = ppr t

instance Pretty CSInt where
  ppr Int8T = text "byte"
  ppr Int16T = text "short"
  ppr Int32T = text "int"
  ppr Int64T = text "long"

instance Pretty CSFloat where
  ppr FloatT = text "float"
  ppr DoubleT = text "double"

data UnOp = Not -- ^ Boolean negation.
          | Complement -- ^ Bitwise complement.
          | Negate -- ^ Numerical negation.
          | Abs -- ^ Absolute/numerical value.
            deriving (Eq, Show)

data CSExp = Integer Integer
           | Bool Bool
           | Float Double
           | String String
           | RawStringLiteral String
           | Var String
           | Ref String
           | Deref String
           | BinOp String CSExp CSExp
           | UnOp String CSExp
           | Cond CSExp CSExp CSExp
           | Index CSExp CSIdx
           | Pair CSExp CSExp
           | Call CSExp [CSArg]
           | CallMethod CSExp CSExp [CSArg]
           | CreateObject CSExp [CSArg]
           | CreateArray CSType [CSExp]
           | Cast CSExp String
           | Tuple [CSExp]
           | Array [CSExp]
           | Field CSExp String
           | Lambda CSExp [CSStmt]
           | Collection String [CSExp]
           | Null
           -- | Dict [(CSExp, CSExp)]
           deriving (Eq, Show)

instance Pretty CSExp where
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
  ppr (Pair e1 e2) = braces(ppr e1 <> comma <> ppr e2)
  ppr (Call fun args) = ppr fun <> parens(commasep $ map ppr args)
  ppr (CallMethod obj method args) = ppr obj <> dot <> ppr method <> parens(commasep $ map ppr args)
  ppr (CreateObject className args) = text "new" <+> ppr className <> parens(commasep $ map ppr args)
  ppr (CreateArray t dims) = text "new[]" <+> braces(commasep $ map ppr dims)
  ppr (Tuple exps) = parens(commasep $ map ppr exps)
  ppr (Array exps) = braces(commasep $ map ppr exps) -- uhoh is this right?
  ppr (Field obj field) = ppr obj <> dot <> text field
  ppr (Lambda expr stmts) = ppr expr <+> text "=>" <+> braces(stack $ map ppr stmts)
  ppr (Collection collection exps) = text "new "<> text collection <> text "()" <> braces(commasep $ map ppr exps)
  ppr Null = text "null"
  --ppr (Dict exps) = undefined

data CSIdx = IdxRange CSExp CSExp
               | IdxExp CSExp
               deriving (Eq, Show)

data CSArg = ArgKeyword String CSArg -- please don't assign multiple keywords with the same argument
           | Arg (Maybe ArgMemType) CSExp
           deriving (Eq, Show)

instance Pretty CSArg where
  ppr (ArgKeyword kw arg) = text kw <> colon <+> ppr arg
  ppr (Arg (Just mt) arg) = ppr mt <+> ppr arg
  ppr (Arg Nothing arg) = ppr arg

data CSStmt = If CSExp [CSStmt] [CSStmt]
            | Try [CSStmt] [CSExcept]
            | While CSExp [CSStmt]
            | For String CSExp [CSStmt]
            | UsingWith CSStmt [CSStmt]
            | Unsafe [CSStmt]

            | Assign CSExp CSExp
            | Reassign CSExp CSExp
            | AssignOp String CSExp CSExp
            | AssignTyped String CSExp CSExp

            | Comment String [CSStmt]
            | Assert CSExp String
            | Throw CSExp
            | Exp CSExp
            | Return CSExp
            | Pass
              -- Definition-like statements.
            | Using (Maybe String) String
            | FunDef CSFunDef
            | ClassDef CSClassDef
            | ConstructorDef CSConstructorDef

              -- Some arbitrary string of CS code.
            | Escape String
                deriving (Eq, Show)

instance Pretty CSStmt where
  ppr (If cond tbranch fbranch) =
    text "if" <+> parens(ppr cond) </>
    lbrace </>
    indent 4 (stack $ map ppr tbranch) </>
    rbrace </>
    text "else" </>
    lbrace </>
    indent 4 (stack $ map ppr fbranch) </>
    rbrace

  ppr (Try stmts excepts) =
    text "try" </>
    lbrace </>
    indent 4 (stack $ map ppr stmts) </>
    rbrace </>
    (stack $ map ppr excepts)

  ppr (While cond body) =
    text "while" <+> parens(ppr cond) </>
    lbrace </>
    indent 4 (stack $ map ppr body) </>
    rbrace

  ppr (For i what body) =
    text "foreach" <+> parens(text "var" <+> text i <+> text "in" <+> ppr what) </>
    lbrace </>
    indent 4 (stack $ map ppr body) </>
    rbrace

  ppr (Using (Just as) from) =
    text "using" <+> text as <+> text "=" <+> text from <> semi

  ppr (Using Nothing from) =
    text "using" <+> text from <> semi

  ppr (Unsafe stmts) =
    text "unsafe" </>
    lbrace </>
    indent 4 (stack $ map ppr stmts) </>
    rbrace

  ppr (UsingWith assignment body) =
    text "using" <+> parens(ppr assignment) </>
    lbrace </>
    indent 4 (stack $ map ppr body) </>
    rbrace

  ppr (Assign e1 e2) = text "var" <+> ppr e1 <+> text "=" <+> ppr e2 <> semi
  ppr (Reassign e1 e2) = ppr e1 <+> text "=" <+> ppr e2 <> semi
  ppr (AssignTyped s e1 e2) = text s <+> ppr e1 <+> text "=" <+> ppr e2 <> semi

  ppr (AssignOp op e1 e2) = ppr e1 <+> text (op ++ "=") <+> ppr e2 <> semi

  ppr (Comment s body) = text "//" <> text s </> stack (map ppr body)

  ppr (Assert e s) =
    text "Debug.Assert" <> parens(ppr e <> text "," <+> squotes(text s)) <> semi

  ppr (Throw e) = text "throw" <+> ppr e <> semi

  ppr (Exp e) = ppr e <> semi

  ppr (Return e) = text "return" <+> ppr e <> semi

  ppr (ClassDef d) = ppr d

  ppr (FunDef d) = ppr d

  ppr (ConstructorDef d) = ppr d

  ppr (Escape s) = stack $ map text $ lines s

  ppr Pass = empty

instance Pretty CSFunDef where
  ppr (Def fname retType args stmts) =
    ppr retType <+> text fname <> parens(commasep(map ppr' args)) </>
    lbrace </>
    indent 4 (stack (map ppr stmts)) </>
    rbrace
    where ppr' (var, tp) = ppr tp <+> text var

instance Pretty CSClassDef where
  ppr (Class cname body) =
    text "class" <+> text cname </>
    lbrace </>
    indent 4 (stack (map ppr body)) </>
    rbrace

instance Pretty CSConstructorDef where
  ppr (ClassConstructor cname params body) =
    text "public" <+> text cname <> parens(commasep $ map ppr params) </>
    lbrace </>
    indent 4 (stack (map ppr body)) </>
    rbrace

instance Pretty CSExcept where
  ppr (Catch csexp stmts) =
    text "catch" <+> parens(ppr csexp <+> text "e") </>
    lbrace </>
    indent 4 (stack (map ppr stmts)) </>
    rbrace

data CSExcept = Catch CSExp [CSStmt]
              deriving (Eq, Show)

type CSFunDefArg = (String, CSType)
data CSFunDef = Def String CSType [CSFunDefArg] [CSStmt]
                  deriving (Eq, Show)

data CSClassDef = Class String [CSStmt]
                deriving (Eq, Show)

data CSConstructorDef = ClassConstructor String [CSFunDefArg] [CSStmt]
                deriving (Eq, Show)

newtype CSProg = CSProg [CSStmt]
                   deriving (Eq, Show)

instance Pretty CSProg where
  ppr (CSProg stms) = stack (map ppr stms)
