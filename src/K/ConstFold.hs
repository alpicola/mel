module K.ConstFold (constFold) where

import Control.Applicative
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Bifunctor

import Frontend.AST
import Frontend.Types
import Frontend.Values
import Frontend.Primitives
import K.AST

import Internal

-- Do constant folding

constFold :: KProgram -> KProgram
constFold = second $ \decls ->
  let initialState = (M.empty, (M.empty, M.empty))
  in catMaybes $ runCF initialState $ mapM cfDecl decls

type CopyState = Map Name Name
type ConstState = (Map Name Value, Map Name (Name, [Name]))
type CF a = State (CopyState, ConstState) a

runCF :: (CopyState, ConstState) -> CF a -> a
runCF state = flip evalState state

addCopy :: Name -> Name -> CF ()
addCopy n n' = modify $ first $ M.insert n n'

addConstValue :: Name -> Value -> CF ()
addConstValue n c = modify $ second $ first $ M.insert n c

addConstData :: Name -> (Name, [Name]) -> CF ()
addConstData n c = modify $ second $ second $ M.insert n c

addConstTuple :: Name -> [Name] -> CF ()
addConstTuple n c = modify $ second $ second $ M.insert n (Erased, c)

getOrig :: Name -> CF Name
getOrig name = gets $ fromMaybe name . M.lookup name . fst

getInt :: Name -> CF (Maybe Int)
getInt name = gets $ (M.lookup name >=> fromIntValue) . fst . snd
 where
  fromIntValue (IntValue i) = Just i
  fromIntValue _ = Nothing

getFloat :: Name -> CF (Maybe Float)
getFloat name = gets $ (M.lookup name >=> fromFloatValue) . fst . snd
 where
  fromFloatValue (FloatValue f) = Just f
  fromFloatValue _ = Nothing

getData :: Name -> CF (Maybe (Name, [Name]))
getData name = gets $ M.lookup name . snd . snd

getTuple :: Name -> CF (Maybe [Name])
getTuple name = gets $ fmap snd . M.lookup name . snd . snd

cfExpr :: KExpr -> CF KExpr
cfExpr (KVar n) = KVar <$> getOrig n
cfExpr (KValue v) = return $ KValue v
cfExpr (KIf op n1 n2 e1 e2) = do
  n1' <- getOrig n1
  n2' <- getOrig n2
  o1 <- sequence <$> mapM getInt [n1', n2']
  o2 <- sequence <$> mapM getFloat [n1', n2']
  case (evalCmpOp op <$> o1) `mplus` (evalCmpOp op <$> o2) of
    Just b -> if b then cfExpr e1 else cfExpr e2
    Nothing -> KIf op n1' n2' <$> cfExpr e1 <*> cfExpr e2
cfExpr (KLet d e) = do
  m <- cfDecl d
  case m of
    Just d' -> KLet d' <$> cfExpr e
    Nothing -> cfExpr e
cfExpr (KMatch n alts) = do
  n' <- getOrig n
  c <- getData n
  case c of
    Just (con, ns) ->
      case fromJust $ find (isMatch con) alts of
        KConCase _ bs e -> do
          mapM_ (uncurry addCopy) (zip ns (map fst bs))
          cfExpr e
        KDefaultCase e -> cfExpr e
    Nothing -> KMatch n' <$> mapM cfAlt alts 
cfExpr (KApply n ns) =
  KApply <$> getOrig n <*> mapM getOrig ns
cfExpr (KOp (Cmp op) ns) = do
  ns' <- mapM getOrig ns
  o1 <- sequence <$> mapM getInt ns'
  o2 <- sequence <$> mapM getFloat ns'
  case (evalCmpOp op <$> o1) `mplus` (evalCmpOp op <$> o2) of
    Just b -> return $ KValue $ IntValue $ if b then 1 else 0 
    Nothing -> return $ KOp (Cmp op) ns'
cfExpr (KOp (Arith op) ns) = do
  ns' <- mapM getOrig ns
  o <- sequence <$> mapM getInt ns'
  case evalArithOp op <$> o of
    Just i -> return $ KValue $ IntValue i
    Nothing -> return $ KOp (Arith op) ns'
cfExpr (KOp (FArith op) ns) = do
  ns' <- mapM getOrig ns
  o <- sequence <$> mapM getFloat ns'
  case evalFArithOp op <$> o of
    Just f -> return $ KValue $ FloatValue f
    Nothing -> return $ KOp (FArith op) ns'
cfExpr (KCon con ns) =
  KCon con <$> mapM getOrig ns
cfExpr (KTuple ns) =
  KTuple <$> mapM getOrig ns
cfExpr (KProj i n) = do
  KProj i <$> getOrig n
cfExpr (KExt s t ns) = do
  KExt s t <$> mapM getOrig ns

cfDecl :: KDecl -> CF (Maybe KDecl) 
cfDecl (KFunDecl b bs e) =
  Just . KFunDecl b bs <$> cfExpr e
cfDecl (KDecl b@(n, _) e) = do
  e' <- cfExpr e
  let d = KDecl b e'
  case e' of
    KVar n' -> do
      Nothing <$ addCopy n n'
    KValue v -> do
      Just d <$ addConstValue n v
    KCon con ns ->
      Just d <$ addConstData n (con, ns)
    KTuple ns ->
      Just d <$ addConstTuple n ns
    KProj i n' -> do
      c <- getTuple n'
      case c of
        Just ns -> Nothing <$ addCopy n (ns !! (i - 1))
        _ -> return $ Just d
    _ -> return $ Just d

cfAlt :: KAlt -> CF KAlt
cfAlt (KConCase con bs e) =
  KConCase con bs <$> cfExpr e
cfAlt (KDefaultCase e) =
  KDefaultCase <$> cfExpr e

isMatch :: Name -> KAlt -> Bool
isMatch con (KConCase con' _ _) = con == con'
isMatch _ (KDefaultCase _) = True
