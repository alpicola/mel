module K.PrettyPrint (prettyPrint) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Data.Char (isSpace)
import Data.Bifunctor

import Frontend.Values
import Frontend.Primitives
import K.AST
import K.Types

import Internal

-- Pretty print

prettyPrint :: KProgram -> String 
prettyPrint (_, decls) = intercalate "\n\n" $ map (runPP . ppDecl) decls

type IsNewline = Bool
type Newlined = Bool
type Indent = Int
type PP a = StateT (IsNewline, Newlined) (ReaderT Indent (Writer String)) a 

runPP :: PP a -> String
runPP = execWriter . flip runReaderT 0 . flip evalStateT (True, False)

indentWidth :: Int
indentWidth = 2

withIndent :: PP a -> PP Newlined
withIndent m = do
  newlined <- gets snd
  modify (second $ const False)
  local (+ 1) m
  newlined' <- gets snd
  modify (second $ const newlined)
  return newlined'

newline :: PP ()
newline = do
  isNewline <- gets fst
  unless isNewline $ do
    indent <- asks $ flip replicate ' ' . (indentWidth *)
    tell $ '\n' : indent
    put (True, True)

write :: String -> PP ()
write s = tell s >> modify (first $ const False)

ppExpr :: KExpr -> PP ()
ppExpr (KVar name) = write $ show name ++ " "
ppExpr (KValue val) = write $ show val ++ " "
ppExpr (KIf cmp n1 n2 e1 e2) = do
  newline
  write $ "if " ++ show n1 ++ " " ++ show cmp ++ " " ++ show n2 ++ " then "
  void $ withIndent (ppExpr e1)
  newline
  write "else "
  void $ withIndent (ppExpr e2)
ppExpr (KLet d e) = do
  newlined <- ppDecl d
  when newlined newline
  write "in "
  ppExpr e
ppExpr (KMatch n alts) = do
  newline
  write $ "match " ++ show n ++ " with "
  mapM_ (withIndent . ppAlt) alts
ppExpr (KApply n ns) =
  write $ intercalate " " (map show (n:ns)) ++ " "
ppExpr (KOp (Cmp cmp) [n1, n2]) =
  write $ show n1 ++ " " ++ show cmp ++ " " ++ show n2 ++ " "
ppExpr (KOp (Arith Minus) [n]) =
  write $ show Minus ++ " " ++ show n ++ " "
ppExpr (KOp (Arith op) [n1, n2]) =
  write $ show n1 ++ " " ++ show op ++ " " ++ show n2 ++ " "
ppExpr (KOp (FArith FMinus) [n]) =
  write $ show Minus ++ " " ++ show n ++ " "
ppExpr (KOp (FArith op) [n1, n2]) =
  write $ show n1 ++ " " ++ show op ++ " " ++ show n2 ++ " "
ppExpr (KCon con ns) = do
  write $ show con ++ " "
  case ns of
    [] -> return ()
    [n] -> write $ show n ++ " "
    _ -> write $ "(" ++ intercalate ", " (map show ns) ++ ") "
ppExpr (KTuple ns) =
  write $ "(" ++ intercalate ", " (map show ns) ++ ") "
ppExpr (KProj i n) =
  write $ "#proj" ++ show i ++ " " ++ show n ++ " "

ppDecl :: KDecl -> PP Newlined
ppDecl (KFunDecl (n, t) bs e) = do
  newline
  write $ "let rec " ++ show n ++ " "
  write $ "(" ++ (intercalate ") (" $ map showBinder bs) ++ ") "
  write $ ": " ++ show (returnType t $ length bs) ++ " = "
  withIndent $ ppExpr e
ppDecl (KDecl b e) = do
  newline
  write $ "let " ++ showBinder b ++ " = "
  withIndent $ ppExpr e

ppAlt :: KAlt -> PP ()
ppAlt (KConCase con bs e) = do
  newline
  write $ "| " ++ show con ++ " "
  unless (null bs) $ do
    write $ "(" ++ (intercalate ", " $ map showBinder bs) ++ ") "
  write "-> "
  void $ withIndent $ ppExpr e
ppAlt (KDefaultCase e) = do
  newline
  write $ "| _ ->"
  void $ withIndent $ ppExpr e

showBinder :: KBinder -> String
showBinder (n, t) = show n ++ ":" ++ show t
