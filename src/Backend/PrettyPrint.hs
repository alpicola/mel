module Backend.PrettyPrint (prettyPrint) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.List

import Frontend.Values
import Frontend.Primitives
import Backend.AST

import Internal

-- Pretty print

prettyPrint :: LProgram -> String 
prettyPrint decls = intercalate "\n\n" $ map (runPP . ppDecl) decls

type IsNewline = Bool
type Indent = Bool
type PP a = StateT IsNewline (ReaderT Indent (Writer String)) a

runPP :: PP a -> String
runPP = execWriter . flip runReaderT False . flip evalStateT True

indentString :: String
indentString = "\t"

withIndent :: PP a -> PP a
withIndent m = local (const True) m

newline :: PP ()
newline = do
  isNewline <- get
  unless isNewline $ do
    indent <- asks $ \i -> if i then indentString else ""
    tell $ '\n' : indent
    put True

write :: String -> PP ()
write s = tell s >> put False

ppDecl :: LDecl -> PP ()
ppDecl (LFunDecl t n args blocks) = do
  newline
  write $ show t ++ " " ++ show n
  write $ "(" ++ intercalate ", " (map showBinder args) ++ ")"
  mapM_ ppBlock blocks

ppBlock :: LBlock -> PP ()
ppBlock (LBlock label args statements transfur) = do
  newline
  write $ show label
  unless (null args) $
    write $ "(" ++ intercalate ", " (map showBinder args) ++ ")"
  write $ ":"
  withIndent $ mapM_ ppStatement statements
  withIndent $ ppTransfur transfur

ppTransfur :: LTransfur -> PP ()
ppTransfur (LReturn n) = do
  newline
  write $ "return " ++ show n
ppTransfur (LIf cmp n1 n2 l1 l2) = do
  newline
  write $ "if " ++ show n1 ++ " " ++ show cmp ++ " " ++ show n2 ++ ", "
  write $ show l1 ++ ", " ++ show l2
ppTransfur (LMatch n alts default') = do
  newline
  write $ "match " ++ show n ++ ", "
  write $ "[" ++ intercalate ", " (map (\(i, l)-> '#' : show i ++ " " ++ show l) alts)  ++ "]" 

ppTransfur (LGoto label ns) = do
  newline
  write $ "goto " ++ show label ++ ", " ++ intercalate ", " (map show ns)
ppTransfur (LTailCall n ns) = do
  newline
  write $ "tailcall " ++ show n ++ ", " ++ intercalate ", " (map show ns)

ppStatement :: LStatement -> PP ()
ppStatement (b, inst) = do
  newline
  write $ showBinder b ++ " <- "
  ppInst inst

ppInst :: LInst -> PP ()
ppInst (LVar n) = write $ show n
ppInst (LValue v) = write $ show v
ppInst (LCall n ns) =
  write $ "call " ++ show n ++ ", " ++ intercalate ", " (map show ns)
ppInst (LOp (Cmp cmp) [n1, n2]) =
  write $ show n1 ++ " " ++ show cmp ++ " " ++ show n2
ppInst (LOp (Arith Minus) [n]) =
  write $ show Minus ++ " " ++ show n ++ " "
ppInst (LOp (Arith op) [n1, n2]) =
  write $ show n1 ++ " " ++ show op ++ " " ++ show n2
ppInst (LOp (FArith FMinus) [n]) =
  write $ show Minus ++ " " ++ show n ++ " "
ppInst (LOp (FArith op) [n1, n2]) =
  write $ show n1 ++ " " ++ show op ++ " " ++ show n2
ppInst (LCon tag ns) = do
  write $ ('#' : show tag) ++ " "
  case ns of
    [] -> return ()
    [n] -> write $ show n
    _ -> write $ "(" ++ intercalate ", " (map show ns) ++ ")"
ppInst (LTuple ns) =
  write $ "(" ++ intercalate ", " (map show ns) ++ ")"
ppInst (LProj i n) =
  write $ "proj" ++ show i ++ " " ++ show n
ppInst (LExtCall s ns) =
  write $ ('%' : s) ++ " " ++ intercalate ", " (map show ns)
ppInst (LCast _ n) =
  write $ "cast" ++ " " ++ show n

showBinder :: LBinder -> String
showBinder (n, t) = show t ++ " " ++ show n
