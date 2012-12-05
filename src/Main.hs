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
-- import Interp
import AST

main = do 
  f:_  <- getArgs
  if null f
    then printUsage
    else do expr <- lparse f
            print expr
  -- print $ evalExp newEnv expr


printUsage :: IO ()
printUsage = do
  putStrLn "island v0.1"
  putStrLn "usage - island <source>"
