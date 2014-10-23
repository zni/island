-- ----------------------------------------------
-- Author: Matthew Godshall
-- Date  : 2011 May 13 18:16:03
--
-- Interpreter driver.
-- ----------------------------------------------
module Main where

import System.Environment (getArgs)

import Env
import Frontend
import Interp
import Types

main = do 
  args <- getArgs
  if null args
    then error "No input file specified."
    else do expr <- lparse $ head args
            putStrLn $ "parsed: " ++ show expr
            putStrLn $ "result: " ++ show (evalExp newEnv expr)
