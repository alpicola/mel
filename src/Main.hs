module Main where

import Control.Monad.Error
import System.Environment

import Frontend.Parser
import Frontend.Elaborate
import Internal

main = do
  args <- getArgs
  case args of
    [file] -> do
      src <- readFile file
      let result = runFresh . runErrorT $ parseMLProgram src >>= elaborate
      putStrLn $ either id (show . snd) result
