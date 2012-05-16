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
             , PrimOp(..)) where

-- ----------------------------------------------
-- Expressions
-- ----------------------------------------------
data Exp =
    Lit Integer          |
    Var String           |
    PrimExp PrimOp [Exp] |
    IfExp Exp Exp Exp    |
    LetExp [Binding] Exp |
    ProcExp [Ident] Exp  |
    AppExp Exp [Exp]     |
    LetRecExp [RecBinding] Exp
    deriving (Show)

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