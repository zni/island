-- ----------------------------------------------
-- Author: Matthew Godshall
-- Date  : 2011 May 12 16:27:26
--
-- AST for the interpreter.
-- ----------------------------------------------
module AST ( AST(..)
           , Lit(..)
           , Binding(..)
           , RecBinding(..)
           , PrimOp(..)
           , TypeName(..)
           , TypeCons(..)
           , Type(..)
           , litToType ) where

-- ----------------------------------------------
-- Expressions
-- ----------------------------------------------
data AST =
    Literal Lit                |
    Var Type String            |
    PrimExp PrimOp [AST]       |
    IfExp AST AST AST          |
    LetExp [Binding] AST       |
    ProcExp [AST] AST          |
    AppExp AST [AST]           | 
    LetRecExp [RecBinding] AST |
    TypeDec TypeName [TypeCons]|
    TopLevel AST [AST] AST
    deriving (Show, Eq)

type TypeName = String
type Binding = (AST, AST)
type RecBinding = (AST, [AST], AST)

-- Literal values
data Lit =
    LString String |
    LInt Integer   |
    LChar Char     |
    LBool Bool
    deriving (Show, Eq)

-- Primitive operations.
data PrimOp =
    Add   |
    Sub   |
    Mult  |
    Div   |
    Mod   |
    Succ  |
    Pred
    deriving (Show, Eq)


-- ----------------------------------------------
-- Types
-- ----------------------------------------------
data Type =
    TVar String      |
    TUser String     |
    TInt             |
    TBool            |
    TString          |
    TChar            |
    TArrow Type Type |
    TBottom
    deriving (Eq)

instance Show Type where
    show (TVar s)                         = s
    show (TInt)                           = "Int"
    show (TBool)                          = "Bool"
    show (TString)                        = "String"
    show (TChar)                          = "Char"
    show (TArrow left@(TArrow _ _) right) = "("++show left++") -> "++show right
    show (TArrow left right)              = show left++" -> "++show right
    show (TBottom)                        = "_|_"

-- Nullary type constructor.
data TypeCons = TypeCons String deriving (Show, Eq)

-- Convenience functions.
litToType :: AST -> Type
litToType (Literal (LString _)) = TString
litToType (Literal (LBool _))   = TBool
litToType (Literal (LInt _))    = TInt
litToType (Literal (LChar _))   = TChar
litToType _                     = TBottom
