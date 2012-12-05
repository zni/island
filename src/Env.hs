-- ----------------------------------------------
-- Author: Matthew Godshall
-- Date  : 2011 May 12 20:38:55
--
-- The environment for the interpreter.
-- XXX When the environment is rewritten Ident will be Var (AST).
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

import AST

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


valueToType :: Value -> Type
valueToType (IntVal _) = TInt
valueToType (CharVal _) = TChar
valueToType (StringVal _) = TString
valueToType (BoolVal _) = TBool
valueToType (ProcVal (Closure t _ _ _)) = t 

-- ----------------------------------------------
-- Closure
-- ----------------------------------------------
data Closure = Closure Type [AST] AST Env


-- ----------------------------------------------
-- Env
-- ----------------------------------------------
type Env = [([VarRef], Store)]
type VarRef = (AST, Int)
type Store = Array Int Value

-- Create a new environment.
newEnv :: Env
newEnv = []

-- Extend the environment.
extendEnv :: [AST] -> [Value] -> Env -> Env
extendEnv d v env =
    let varrefs = zip d [0..(length d - 1)]
        arr     = listArray (0, pred $ length d) v
    in (varrefs, arr):env

-- Extend the environment with a recursive binding.
extendEnvRec :: [RecBinding] -> Env -> Env
extendEnvRec recs env =
    let procIds = map (\(p,_,_) -> p) recs
        varrefs = zip procIds [0..(length procIds - 1)]
        arr     = listArray (0, pred $ length procIds) $ mapClosure recs env2
        env2    = (varrefs, arr):env
    in env2
    where mapClosure :: [RecBinding] -> Env -> [Value]
          -- XXX Fix the type of Closure.
          mapClosure xs e = map (\(_, ids, body) -> ProcVal $ Closure TBottom ids body e) xs

-- Return a value from the environment.
applyEnv :: AST -> Env -> (Value, Type)
applyEnv v []     = 
    let (Var _ s) = v
    in error $ "Variable \'"++s++"\' was never declared."

applyEnv v ((vars, d):ds) =
    let loc = findRef v vars
    in if isJust loc 
           then let val   = d ! fromJust loc
                    typeN = valueToType val
                in (val, typeN)
           else applyEnv v ds

findRef :: AST -> [VarRef] -> Maybe Int
findRef _ []          = Nothing
findRef s ((n, l):xs) = 
  if s == n 
    then Just l
    else findRef s xs
