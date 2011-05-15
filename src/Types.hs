-- ----------------------------------------------
-- Author: Matthew Godshall
-- Date  : 2011 May 12 16:27:26
--
-- Types for the interpreter.
-- ----------------------------------------------
module Types where

-- ----------------------------------------------
-- Expressions
-- ----------------------------------------------
data Exp =
    Lit Integer          |
    Var String           |
    UnExp UnOp Exp       |
    BinExp BinOp Exp Exp |
    IfExp Exp Exp Exp    |
    LetExp [Binding] Exp |
    ProcExp [Ident] Exp  |
    AppExp Exp [Exp]
    deriving (Show)

type Ident = String
type Binding = (Ident, Exp)

-- Unary operators.
data UnOp =
    Minus |
    Inc   |
    Dec
    deriving (Show)


-- Binary operators.
data BinOp =
    Add   |
    Sub   |
    Mult  |
    Div   |
    Mod
    deriving (Show)
