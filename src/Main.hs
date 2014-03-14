module Main where

import Control.Monad.Error
import System.Environment
import Data.Map (Map)
import qualified Data.Map as M
import Data.Bitraversable

import Frontend.AST
import Frontend.Dianostic
import Frontend.Parser
import Frontend.Elaborate
import Frontend.Alpha
import Frontend.Monomorphise

import K.AST
import K.Normalize
import K.Optimize
import K.Globalize
import K.Defunctionalize
import K.PrettyPrint

import Internal

import Paths_mel

main = do
  args <- getArgs
  case args of
    [file] -> do
      libSrc <- getDataFileName libFilePath >>= readFile
      src <- readFile file
      putStrLn $ either id prettyPrint $ do
        prog <- parse (libSrc ++ src) >>= elaborate
        return . runFresh $ compile prog
 where
  libFilePath = "lib/pervasives.ml"

compile :: AnnProgram -> Fresh KProgram
compile = bimapM return (alpha M.empty)
      >=> monomorphise
      >=> normalize
      >=> optimize
      >=> globalize
      >=> defunctionalize
      >=> optimize
