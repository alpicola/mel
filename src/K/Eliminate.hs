module K.Eliminate (eliminate) where

import Control.Applicative
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as S
import Data.Bifunctor
import Data.Bitraversable

import K.AST

import Internal

-- Eliminate unused declarations

eliminate :: KProgram -> KProgram
eliminate = second $ \decls ->
  -- it must perform in a reverse order to find if there exists use
  let decls' = runElim $ reverse <$> mapM elimDecl (reverse decls)
  in filter (not . isUseless) decls'

type Elim a = State (Set Name) a

runElim :: Elim a -> a
runElim = flip evalState S.empty 

useOf :: Name -> Elim Bool
useOf name = gets $ S.member name

use :: Name -> Elim ()
use name = modify $ S.insert name

elimBinder :: KBinder -> Elim KBinder
elimBinder = flip bimapM return $ \n ->
  case n of
    Special _ -> return n
    _ -> do
      isUsed <- useOf n
      return $ if isUsed then n else Erased

elimExpr :: KExpr -> Elim KExpr
elimExpr (KVar n) = KVar n <$ use n
elimExpr (KValue val) = return $ KValue val
elimExpr (KIf cmp n1 n2 e1 e2) = do
  use n1
  use n2
  KIf cmp n1 n2 <$> elimExpr e1 <*> elimExpr e2
elimExpr (KMatch n alts) = do
  use n
  KMatch n <$> mapM elimAlt alts
elimExpr (KLet d e) = do
  e' <- elimExpr e
  d' <- elimDecl d
  return $ if isUseless d' then e' else KLet d' e'
elimExpr (KApply n ns) = KApply n ns <$ mapM_ use (n:ns)
elimExpr (KOp op ns) = KOp op ns <$ mapM_ use ns
elimExpr (KCon con ns) = KCon con ns <$ mapM_ use ns
elimExpr (KTuple ns) = KTuple ns <$ mapM_ use ns
elimExpr (KProj i n) = KProj i n <$ use n

elimDecl :: KDecl -> Elim KDecl
elimDecl (KFunDecl b bs e) = do
  b' <- elimBinder b
  e' <- elimExpr e
  bs' <- mapM elimBinder bs
  return $ KFunDecl b' bs' e'
elimDecl (KDecl b e) =
  KDecl <$> elimBinder b <*> elimExpr e
 
elimAlt :: KAlt -> Elim KAlt
elimAlt (KConCase con bs e) = do
  e' <- elimExpr e
  bs' <- mapM elimBinder bs
  return $ KConCase con bs' e'
ueAlt (KDefaultCase e) =
  KDefaultCase <$> elimExpr e

isUseless :: KDecl -> Bool
isUseless (KFunDecl (n, _) _ _) = isErased n
isUseless (KDecl (n, _) e) = isErased n && not (hasSideEffects e)
