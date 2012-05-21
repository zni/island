-- ----------------------------------------------
-- Author: Matthew Godshall
-- Date  : 2011 May 12 16:27:26
--
-- Types for the interpreter.
-- ----------------------------------------------
module Types ( Exp(..)
             , Ident(..)
             , Binding(..)
             , RecBinding(..)
             , PrimOp(..)
             , TypeName(..)
             , TypeCons(..)) where

-- ----------------------------------------------
-- Expressions
-- ----------------------------------------------
data Exp =
    LitString String           |
    LitType   TypeName         |
    LitInt    Integer          |
    LitChar   Char             |
    Var String                 |
    PrimExp PrimOp [Exp]       |
    IfExp Exp Exp Exp          |
    LetExp [Binding] Exp       |
    ProcExp [Ident] Exp        |
    AppExp Exp [Exp]           |
    LetRecExp [RecBinding] Exp |
    TypeDec TypeName [TypeCons]
    deriving (Show)

type TypeName = String
type Ident = String
type Binding = (Ident, Exp)
type RecBinding = (Ident, [Ident], Exp)

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
-- Type Constructors
-- ----------------------------------------------
data TypeCons = TypeCons Ident deriving (Show)