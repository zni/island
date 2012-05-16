-- ----------------------------------------------
-- Author: Matthew Godshall
-- Date  : 2011 May 12 20:38:55
--
-- The environment for the interpreter.
-- ----------------------------------------------
module Env ( Value(..)
           , Closure(..)
           , Env(..)
           , newEnv
           , extendEnv
           , extendEnvRec
           , applyEnv
           ) where

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

extendEnv :: [Ident] -> [Value] -> Env -> Env
extendEnv d v env =
    let varrefs = zip d [0..(length d - 1)]
        arr     = listArray (0, pred $ length d) v
    in (varrefs, arr):env

extendEnvRec :: [RecBinding] -> Env -> Env
extendEnvRec recs env =
    let procIds = map (\(p,_,_) -> p) recs
        varrefs = zip procIds [0..(length procIds - 1)]
        arr     = listArray (0, pred $ length procIds) $ mapClosure recs env2
        env2    = (varrefs, arr):env
    in env2
    where mapClosure :: [RecBinding] -> Env -> [Value]
          mapClosure xs e = map (\(_,ids,body) -> ProcVal $ Closure ids body e) xs

applyEnv :: Ident -> Env -> Value
applyEnv v []     = error $ "Variable \'"++v++"\' never declared."
applyEnv v ((vars,d):ds) =
    let loc = findRef v vars
    in if isJust loc then d ! fromJust loc else applyEnv v ds
    where findRef _ []         = Nothing
          findRef s ((n,l):xs) = if s == n then Just l else findRef s xs

