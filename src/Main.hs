module Main where

import Control.Monad.Error
import System.Environment

import Frontend.AST
import Frontend.Dianostic
import Frontend.Parser
import Frontend.Elaborate
import Frontend.Alpha
import Internal

main = do
  args <- getArgs
  case args of
    [file] -> do
      src <- readFile file
      putStrLn $ either id (show . snd) $ runFrontend src
 where
  runFrontend :: String -> Either Dianostic AnnProgram
  runFrontend src = do
    prog <- parseMLProgram src >>= runFresh . runErrorT . elaborate
    return $ runFresh $ alpha prog
