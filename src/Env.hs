-- ----------------------------------------------
-- Author: Matthew Godshall
-- Date  : 2011 May 12 20:38:55
--
-- The environment for the interpreter.
-- ----------------------------------------------
module Env where

import Data.Maybe (isJust, fromJust)
import Data.Array.IArray

import Types

-- ----------------------------------------------
-- Values
-- ----------------------------------------------
data Value =
    IntVal Integer           |
    CharVal Char             |
    StringVal String         |
    BoolVal Bool             |
    ProcVal Closure

instance Show Value where
    show (IntVal i)    = show i
    show (CharVal c)   = show c
    show (StringVal s) = s
    show (BoolVal b)   = show b
    show (ProcVal _)   = "ProcVal"


-- ----------------------------------------------
-- Closure
-- ----------------------------------------------
data Closure = Closure [Ident] Exp Env


-- ----------------------------------------------
-- Env
-- ----------------------------------------------
type Env = [([VarRef], Store)]
type VarRef = (String, Int)
type Store = Array Int Value


newEnv :: Env
newEnv = []

extendEnv       :: [String] -> [Value] -> Env -> Env
extendEnv d v env =
    let varrefs = zip d [0..((length d) - 1)]
        arr     = listArray (0, pred $ length d) v
    in (varrefs, arr):env

applyEnv :: String -> Env -> Value
applyEnv v []     = error $ "Variable \'"++v++"\' never declared."
applyEnv v ((vars,d):ds) =
    let loc = findRef v vars
    in if isJust loc then d ! (fromJust loc) else applyEnv v ds
    where findRef _ []         = Nothing
          findRef s ((n,l):xs) = if s == n then Just l else findRef s xs
