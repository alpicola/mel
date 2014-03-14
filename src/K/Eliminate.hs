module K.Eliminate (eliminate) where

import Control.Applicative
import Control.Monad.State
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Bifunctor
import Data.Bitraversable

import K.AST

import Internal

-- Eliminate unused declarations
-- TODO: Eliminate unused constructors

eliminate :: KProgram -> KProgram
eliminate = second $ runElim (S.singleton (Special "main")) . elimToplevel

type Elim a = State (Set Name) a

runElim :: Set Name -> Elim a -> a
runElim state = flip evalState state

useOf :: Name -> Elim Bool
useOf name = gets $ S.member name

addUse :: Name -> Elim ()
addUse name = modify $ S.insert name

elimToplevel :: [KDecl] -> Elim [KDecl]
elimToplevel decls = go decls
 where
  go decls = do
    use <- get
    let (decls1, decls2) = partition (flip S.member use . fst . bindOf) decls
    case decls1 of
      [] -> return [] 
      _ -> flip (++) <$> mapM elimDecl decls1 <*> go decls2

elimBinder :: KBinder -> Elim KBinder
elimBinder = flip bimapM return $ \n ->
  case n of
    Special _ -> return n
    _ -> do
      isUsed <- useOf n
      return $ if isUsed then n else Erased

elimExpr :: KExpr -> Elim KExpr
elimExpr (KVar n) = KVar n <$ addUse n
elimExpr (KValue val) = return $ KValue val
elimExpr (KIf cmp n1 n2 e1 e2) = do
  addUse n1
  addUse n2
  KIf cmp n1 n2 <$> elimExpr e1 <*> elimExpr e2
elimExpr (KMatch n alts) = do
  addUse n
  KMatch n <$> mapM elimAlt alts
elimExpr (KLet d e) = do
  e' <- elimExpr e
  d' <- elimDecl d
  return $ if isUseless d' then e' else KLet d' e'
elimExpr (KApply n ns) = KApply n ns <$ mapM_ addUse (n:ns)
elimExpr (KOp op ns) = KOp op ns <$ mapM_ addUse ns
elimExpr (KCon con ns) = KCon con ns <$ mapM_ addUse ns
elimExpr (KTuple ns) = KTuple ns <$ mapM_ addUse ns
elimExpr (KProj i n) = KProj i n <$ addUse n
elimExpr (KExt s t ns) = KExt s t ns <$ mapM addUse ns

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
elimAlt (KDefaultCase e) =
  KDefaultCase <$> elimExpr e

isUseless :: KDecl -> Bool
isUseless (KFunDecl (n, _) _ _) = isErased n
isUseless (KDecl (n, _) e) = isErased n && not (hasSideEffects e)
