module Main where

import Control.Monad.Error
import System.Environment

import Frontend.AST
import Frontend.Dianostic
import Frontend.Parser
import Frontend.Elaborate
import Frontend.Alpha
import Frontend.Monomorphise

import K.AST
import K.Normalize
import K.ConstFold
import K.PrettyPrint

import Internal

main = do
  args <- getArgs
  case args of
    [file] -> do
      src <- readFile file
      putStrLn $ either id prettyPrint $ do
        prog <- parseMLProgram src >>= elaborate
        return . runFresh $ alpha prog >>= monomorphise >>= normalize >>= return . constFold
