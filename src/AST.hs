-- ----------------------------------------------
-- Author: Matthew Godshall
-- Date  : 2011 May 12 16:27:26
--
-- AST for the interpreter.
-- ----------------------------------------------
module AST ( AST(..)
           , Ident(..)
           , Binding(..)
           , RecBinding(..)
           , PrimOp(..)
           , TypeName(..)
           , TypeCons(..)
           , Type(..)) where

-- ----------------------------------------------
-- Expressions
-- ----------------------------------------------
data AST =
    Literal Lit                |
    Var String                 |
    PrimExp PrimOp [AST]       |
    IfExp AST AST AST          |
    LetExp [Binding] AST       |
    ProcExp [Ident] AST        |
    AppExp AST [AST]           | 
    LetRecExp [RecBinding] AST |
    TypeDec TypeName [TypeCons]|
    TopLevel Ident AST
    deriving (Show)

data Ident = Ident String (Maybe Type)
             deriving (Show)

type TypeName = String
type Binding = (Ident, AST)
type RecBinding = (Ident, [Ident], AST)

-- Literal values
data Lit =
    LString String |
    LInt Integer   |
    LChar Char     |
    LBool Bool
    deriving (Show)

-- Primitive operations.
data PrimOp =
    Add   |
    Sub   |
    Mult  |
    Div   |
    Mod   |
    Succ  |
    Pred
    deriving (Show)


-- ----------------------------------------------
-- Types
-- ----------------------------------------------
data Type =
    TypeVar String |
    Int            |
    Bool           |
    Arrow Type Type

instance Show Type where
    show (TypeVar s)                    = s
    show (Int)                          = "Int"
    show (Bool)                         = "Bool"
    show (Arrow left@(Arrow _ _) right) = "("++show left++") -> "++show right
    show (Arrow left right)             = show left++" -> "++show right

-- Nullary type constructor.
data TypeCons = TypeCons String deriving (Show)
