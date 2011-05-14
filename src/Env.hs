-- ----------------------------------------------
-- Author: Matthew Godshall
-- Date  : 2011 May 12 20:38:55
--
-- The environment for the interpreter.
-- ----------------------------------------------
module Env where

import Data.Maybe (isJust, fromJust)
import Data.Array.IArray

type VarRef = (String, Int)
type DValue = Integer
type Store = Array Int DValue

{- Not yet ready to be unleashed upon the world.
data Value =
    IntVal Integer           |
    CharVal Char             |
    StringVal String         |
    BoolVal Bool             |
    ProcVal ([Exp] -> Value)
    deriving (Show)
-}

type Env = [([VarRef], Store)]

newEnv :: Env
newEnv = []

extendEnv       :: [String] -> [DValue] -> Env -> Env
extendEnv d v env =
    let varrefs = zip d [0..((length d) - 1)]
        arr     = listArray (0, pred $ length d) v
    in (varrefs, arr):env

applyEnv :: String -> Env -> DValue
applyEnv v []     = error $ "Variable "++v++" never declared."
applyEnv v ((vars,d):ds) =
    let loc = findRef v vars
    in if isJust loc then d ! (fromJust loc) else applyEnv v ds
    where findRef _ []         = Nothing
          findRef s ((n,l):xs) = if s == n then Just l else findRef s xs
