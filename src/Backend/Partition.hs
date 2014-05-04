module Backend.Partition where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Traversable as T
import Data.Bifunctor

import Frontend.Values
import Frontend.Primitives

import K.AST

import Backend.AST

import Internal

-- Partition each function body into labeled blocks

partition :: KProgram -> Fresh LProgram
partition (cons, decls) = do
  let conEnv = M.fromList cons
  forM decls $ \(KFunDecl (label, t) bs e) -> do
    let t' = snd $ uncurryType t (length bs)
    (_, blocks) <- runPart conEnv t' $ partExpr e
    return $ LFunDecl t' label bs blocks

type ConEnv = Map Name Int
type Cont = (Maybe Label, LType)
data BlockInfo = BlockInfo Name [LBinder] Cont
type Part a = ReaderT (ConEnv, BlockInfo) (StateT [LStatement] (WriterT [LBlock] Fresh)) a

runPart :: ConEnv -> LType -> Part a -> Fresh (a, [LBlock])
runPart conEnv t = runWriterT . flip evalStateT [] . flip runReaderT (conEnv, entryBlockInfo t)

entryBlockInfo :: LType -> BlockInfo
entryBlockInfo t = BlockInfo (Special "entry") [] (Nothing, t)

withNewBlock :: Label -> [LBinder] -> Maybe (Label, LType) -> Part a -> Part a
withNewBlock label args (Just k) m = do
  put []
  local (second $ const $ BlockInfo label args (first Just k)) m
withNewBlock label args Nothing m = do
  put []
  local (second $ \(BlockInfo _ _ k) -> BlockInfo label args k) m

exitBlock :: LTransfur -> Part ()
exitBlock transfur = do
  statements <- gets reverse
  (BlockInfo label args _) <- asks snd
  tell $ [LBlock label args statements transfur]

addStatement :: LBinder -> LInst -> Part ()
addStatement b i = modify $ (:) (b, i)

partExpr :: KExpr -> Part ()
partExpr (KLet (KDecl b (KIf cmp n1 n2 e1 e2)) e) = do
  l <- Renamed "l" <$> fresh
  partIf cmp n1 n2 e1 e2 $ Just (l, snd b)
  withNewBlock l [b] Nothing $ partExpr e
partExpr (KLet (KDecl b (KMatch n alts)) e) = do
  l <- Renamed "l" <$> fresh
  partMatch n alts $ Just (l, snd b)
  withNewBlock l [b] Nothing $ partExpr e
partExpr (KLet (KDecl b (KLet d e1)) e2) =
  partExpr $ KLet d (KLet (KDecl b e1) e2)
partExpr (KLet (KDecl b e1) e2) = do
  i <- asks $ flip toInst e1 . fst
  addStatement b i
  partExpr e2
partExpr (KLet (KFunDecl _ _ _) _) = undefined
partExpr (KIf cmp n1 n2 e1 e2) =
  partIf cmp n1 n2 e1 e2 Nothing
partExpr (KMatch n alts) =
  partMatch n alts Nothing
partExpr e = do
  i <- asks $ flip toInst e . fst
  (BlockInfo _ _ (k, t)) <- asks snd
  case (i, k) of
    (LCall n ns, Nothing) ->
      exitBlock $ LTailCall n ns
    _ -> do
      n <- Renamed (prefixOfType t) <$> fresh
      addStatement (n, t) i
      exitBlock $ maybe (LReturn n) (flip LGoto [n]) k

partIf :: CmpOp -> Name -> Name -> KExpr -> KExpr -> Maybe LBinder -> Part ()
partIf cmp n1 n2 e1 e2 k = do
  l1 <- Renamed "then" <$> fresh
  l2 <- Renamed "else" <$> fresh
  exitBlock $ LIf cmp n1 n2 l1 l2
  withNewBlock l1 [] k $ partExpr e1
  withNewBlock l2 [] k $ partExpr e2

partMatch :: Name -> [KAlt] -> Maybe LBinder -> Part ()
partMatch n alts k = do
  let (alts', default') = case findIndex isDefaultCase alts of
                            Just i -> (take i alts, Just (alts !! i))
                            Nothing -> (alts, Nothing)
  labels <- mapM (const $ Renamed "case" <$> fresh) alts' 
  label <- T.mapM (const $ Renamed "default" <$> fresh) default'
  conEnv <- asks fst
  let tags = map (\(KConCase con _ _) -> fromJust $ M.lookup con conEnv) alts'
  exitBlock $ LMatch n (zip tags labels) label
  forM_ (zip alts' labels) $ \(KConCase _ bs e, l) ->
    withNewBlock l [] k $ do
      n' <- flip rename n <$> fresh
      let t = TupleType $ IntType : map snd bs
      addStatement (n', t) $ LCast t n
      forM_ (zip bs [2..]) $ \(b, i) -> do
        addStatement b $ LProj i n'
      partExpr e
  void $ T.forM ((,) <$> default' <*> label) $ \(KDefaultCase e, l) ->
    withNewBlock l [] k $ partExpr e

toInst :: ConEnv -> KExpr -> LInst
toInst _ (KVar n) = LVar n
toInst _ (KValue v) = LValue v
toInst _ (KApply n ns) = LCall n ns
toInst _ (KOp op ns) = LOp op ns
toInst env (KCon con ns) = LCon (fromJust $ M.lookup con env) ns
toInst _ (KTuple ns) = LTuple ns
toInst _ (KProj i ns) = LProj i ns
toInst _ (KExt s _ ns) = LExtCall s ns
toInst _ _ = undefined
