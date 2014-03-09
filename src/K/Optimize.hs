module K.Optimize (optimize) where

import Control.Applicative
import Control.Monad
import Data.Bifunctor

import K.AST
import K.ConstFold
import K.UnusedElim
import K.Inline

import Internal

optimize :: KProgram -> Fresh KProgram
optimize prog = go prog iterationLimit
 where
  optimize' = inline >=> return . second flattenLet . constFold . unusedElim
  go prog 0 = return prog
  go prog i = do
    prog' <- optimize' prog
    if prog == prog'
      then return prog
      else go prog' (i - 1)

iterationLimit :: Int
iterationLimit = 100
