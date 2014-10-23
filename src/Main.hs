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
  f:_  <- getArgs
  expr <- lparse f
  print expr
  print $ evalExp newEnv expr
