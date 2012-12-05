-- ----------------------------------------------
-- Author  : Matthew Godshall
-- Date    : 2011 May 12 20:38:55
--
-- The interpreter.
-- ----------------------------------------------
module Interp (evalExp) where

import Env
import AST


-- ----------------------------------------------
-- Evaluation
-- ----------------------------------------------
evalExp :: Env -> AST -> Value

-- Evaluate a literal expression.
evalExp _ (Lit i)   = IntVal i

-- Evaluate a variable expression.
evalExp env (Var s) = applyEnv s env

-- Evaluate a primitive expression.
evalExp env (PrimExp op exps) =
    case op of
      Add  -> IntVal $ addOp  (evalExpList env exps) 0
      Sub  -> IntVal $ subOp  (evalExpList env exps)
      Mult -> IntVal $ multOp (evalExpList env exps) 1
      Div  -> IntVal $ divOp  (evalExpList env exps)
      Mod  -> IntVal $ modOp  (evalExpList env exps)
      Succ -> IntVal $ succOp (evalExpList env exps)
      Pred -> IntVal $ predOp (evalExpList env exps)
    where addOp :: [Value] -> Integer -> Integer
          addOp [] acc = acc
          addOp (IntVal x:xs) acc = 
              let acc' = x + acc
              in seq acc' $ addOp xs acc'

          subOp :: [Value] -> Integer
          subOp (IntVal x:[]) = negate x
          subOp (IntVal x:IntVal y:[]) = x - y
          subOp _                      = error "primitive operation (-): invalid number of arguments."

          multOp :: [Value] -> Integer -> Integer
          multOp [] acc = acc
          multOp (IntVal x:xs) acc = 
              let acc' = x * acc
              in seq acc' $ multOp xs (x * acc)

          divOp :: [Value] -> Integer
          divOp (IntVal x:IntVal y:[]) = x `div` y
          divOp _                      = error "primitive operation (/): invalid number of arguments."

          modOp :: [Value] -> Integer
          modOp (IntVal x:IntVal y:[]) = x `mod` y
          modOp _                      = error "primitive operation (%): invalid number of arguments."

          succOp :: [Value] -> Integer
          succOp (IntVal x:[]) = succ x
          succOp _             = error "primitive operation (succ): invalid number of arguments."

          predOp :: [Value] -> Integer
          predOp (IntVal x:[]) = pred x
          predOp _             = error "primitive operation (pred): invalid number of arguments."


-- Evaluate an if expression.
evalExp env (IfExp test t e) =
    if p (evalExp env test)
        then evalExp env t
        else evalExp env e

    -- Predicate for 'if' expression.
    where p :: Value -> Bool
          p (IntVal n) = n > 0
          p _          = undefined

-- Evaluate a let expression.
evalExp env (LetExp bindings body) =
    let (ids, exps) = unzip bindings
        vals        = evalExpList env exps
        env2        = extendEnv ids vals env
    in evalExp env2 body

-- Evaluate a recursive let expression.
evalExp env (LetRecExp recbindings body) = 
    let recEnv = extendEnvRec recbindings env
    in evalExp recEnv body

-- Evaluate a procedure expression.
evalExp env (ProcExp ids body) = ProcVal (Closure ids body env)

-- Evaluate an applicative expression.
evalExp env (AppExp op args) =
    case op of
        (Var v) -> let operands = evalExpList env args
                       val = applyEnv v env
                   in case val of
                        (ProcVal (Closure ids body env2)) -> evalExp (extendEnv ids operands env2) body
                        _                                 -> appError



        (ProcExp ids body) -> let operands = evalExpList env args
                              in evalExp (extendEnv ids operands env) body
        _                  -> appError

    where appError :: a
          appError = error "Attempting to apply non-procedure to operands."


evalProgram :: [AST] -> Value
evalProgram exprs = evalProgram' exprs newEnv NullValue
    where evalProgram' [AST] -> Env -> Value -> Value
          evalProgram' [] _ value = value
          evalProgram' (x:xs) env value = let value' = evalExp env x

-- ----------------------------------------------
-- Utility functions
-- ----------------------------------------------
evalExpList :: Env -> [AST] -> [Value]
evalExpList env = map (evalExp env)
