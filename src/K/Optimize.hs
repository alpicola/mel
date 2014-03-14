module K.Optimize (optimize) where

import Control.Applicative
import Control.Monad
import Data.Bifunctor

import K.AST
import K.ConstFold
import K.Eliminate
import K.Inline

import Internal

optimize :: KProgram -> Fresh KProgram
optimize prog = go prog iterationLimit
 where
  optimize' = inline >=> (return . flatten . eliminate . constFold)
  go prog 0 = return prog
  go prog i = do
    prog' <- optimize' prog
    if prog == prog'
      then return prog
      else go prog' (i - 1)

iterationLimit :: Int
iterationLimit = 100

flatten :: KProgram -> KProgram
flatten = second $ map flattenDecls
 where
  flattenDecls (KFunDecl b bs e) = KFunDecl b bs $ flattenLet e
  flattenDecls (KDecl b e) = KDecl b $ flattenLet e
