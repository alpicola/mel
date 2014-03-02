module Main where

import Control.Monad.Error
import System.Environment

import Frontend.AST
import Frontend.Dianostic
import Frontend.Parser
import Frontend.Elaborate
import Frontend.Alpha
import Frontend.Monomorphise

import Internal

main = do
  args <- getArgs
  case args of
    [file] -> do
      src <- readFile file
      putStrLn $ either show (show . snd) $ do
        prog <- parseMLProgram src >>= runFresh . runErrorT . elaborate
        return . runFresh $ alpha prog >>= monomorphise
