-- ----------------------------------------------
-- Author  : Matthew Godshall
-- Date    : 2011 May 12 20:38:55
-- Updated : 2011 May 15 03:50:42
--
-- The interpreter.
-- ----------------------------------------------
module Interp where

import Env
import Types


-- ----------------------------------------------
-- Evaluation
-- ----------------------------------------------
evalExp :: Exp -> Env -> Value

-- Lit
evalExp (Lit i) _   = IntVal i

-- Var
evalExp (Var s) env = applyEnv s env

-- UnExp
evalExp (UnExp unop exp) env =
    case unop of
        Minus -> negateV (evalExp exp env)
        _     -> undefined
    where negateV :: Value -> Value 
          negateV (IntVal n) = IntVal $ negate n
          negateV _          = undefined

-- BinExp
evalExp (BinExp binop left right) env =
    case binop of
        Add -> add (evalExp left env) (evalExp right env)
        Sub -> sub (evalExp left env) (evalExp right env)
        Mult -> mult (evalExp left env) (evalExp right env)
        Div -> divV (evalExp left env) (evalExp right env)
        Mod -> modV (evalExp left env) (evalExp right env)

    -- Functions to perform arithmetic operations on 'Value's.
    where add :: Value -> Value -> Value
          add (IntVal x) (IntVal y) = IntVal $ x + y
          add _ _                   = undefined

          sub :: Value -> Value -> Value
          sub (IntVal x) (IntVal y) = IntVal $ x - y
          sub _ _                   = undefined

          mult :: Value -> Value -> Value
          mult (IntVal x) (IntVal y) = IntVal $ x * y
          mult _ _                   = undefined

          divV :: Value -> Value -> Value
          divV (IntVal x) (IntVal y) = IntVal $ div x y
          divV _ _                   = undefined

          modV :: Value -> Value -> Value
          modV (IntVal x) (IntVal y) = IntVal $ mod x y
          modV _ _                   = undefined

-- IfExp
evalExp (IfExp test t e) env =
    if p (evalExp test env)
        then evalExp t env
        else evalExp e env

    -- Predicate for 'if' expression.
    where p :: Value -> Bool
          p (IntVal n) = n > 0
          p _          = undefined

-- LetExp
evalExp (LetExp bindings body) env =
    let (ids, exps) = unzip bindings
        vals        = evalExpList exps env
        env2        = extendEnv ids vals env
    in evalExp body env2

-- LetRecExp
evalExp (LetRecExp recbindings body) env = 
    let recenv = extendEnvRec recbindings env
    in evalExp body recenv

-- ProcExp
evalExp (ProcExp ids body) env = ProcVal (Closure ids body env)

-- AppExp
evalExp (AppExp op args) env =
    case op of
        (Var v) -> let operands = evalExpList args env
                       val = applyEnv v env
                   in case val of
                        (ProcVal (Closure ids body env2)) -> evalExp body (extendEnv ids operands env2)
                        _                                 -> appError



        (ProcExp ids body) -> let operands = evalExpList args env
                              in evalExp body (extendEnv ids operands env)
        _                  -> appError

    where appError :: a
          appError = error "Attempting to apply non-procedure to operands."



-- ----------------------------------------------
-- Utility functions
-- ----------------------------------------------
evalExpList :: [Exp] -> Env -> [Value]
evalExpList exps env = map (\x -> evalExp x env) exps
