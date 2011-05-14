-- ----------------------------------------------
-- Author: Matthew Godshall
-- Date  : 2011 May 12 20:38:55
--
-- The interpreter.
-- ----------------------------------------------
module Interp where

import Env
import Types

evalExp :: Exp -> Env -> Integer

-- Lit
evalExp (Lit i) _   = i

-- Var
evalExp (Var s) env = applyEnv s env

-- UnExp
evalExp (UnExp unop exp) env =
    case unop of
        Minus -> negate (evalExp exp env)
        Inc   -> succ (evalExp exp env)
        Dec   -> pred (evalExp exp env)

-- BinExp
evalExp (BinExp binop left right) env =
    case binop of
        Add -> (+) (evalExp left env) (evalExp right env)
        Sub -> (-) (evalExp left env) (evalExp right env)
        Mult -> (*) (evalExp left env) (evalExp right env)
        Div -> div (evalExp left env) (evalExp right env)
        Mod -> mod (evalExp left env) (evalExp right env)

-- IfExp
evalExp (IfExp test t e) env =
    if (evalExp test env) > 0
        then evalExp t env
        else evalExp e env

-- LetExp
evalExp (LetExp bindings body) env =
    let (ids, exps) = unzip bindings
        vals        = map (\x -> evalExp x env) exps
        env2        = extendEnv ids vals env
    in evalExp body env2

{-
-- ProcExp
evalExp (ProcExp ids body) env = (closure ids body env)

-- AppExp
evalExp (AppExp op args) env =

closure ids body env args =
-}
