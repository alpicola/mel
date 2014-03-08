module Frontend.Parser (parseMLProgram, parseMLExpr) where

import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad.Error
import Control.Monad.Identity
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as T
import Data.Either
import Data.List
import Data.Bifunctor

import Frontend.AST
import Frontend.Values
import Frontend.Primitives
import Frontend.Dianostic
import Internal

import GHC.Float (double2Float)

runParser' :: Parser a -> String -> Either Dianostic a
runParser' p = first show . parse (whiteSpace *> p <* eof) "(input)"

parseMLProgram :: String -> Either Dianostic MLProgram
parseMLProgram = runParser' program

parseMLExpr :: String -> Either Dianostic MLExpr
parseMLExpr = runParser' expr

-- Parser

program :: Parser MLProgram
program = fmap partitionEithers $ sepEndBy1 decl' $ optional $ symbol ";;"
 where
  decl' = Left <$> tdecl
      <|> Right <$> (try (MLDecl Erased <$> expr) <|> decl)

tdecl :: Parser MLTypeDecl
tdecl = (,,) <$ reserved "type"
             <*> option [] ((:[]) <$> tvar <|> parens (sepBy1 tvar comma))
             <*> lowerName <* symbol "="
             <*> sepBy1 constr (symbol "|")

constr :: Parser (Name, [MLType])
constr = (,) <$> upperName
             <*> option [] (reserved "of" *> sepEndBy1 tterm (symbol "*"))

texpr :: Parser MLType
texpr = chainr1 (tuple <$> sepBy1 tterm (symbol "*")) (MLFunType <$ symbol "->")
 where
  tuple [t] = t
  tuple ts = MLTupleType ts

tterm :: Parser MLType
tterm = do
  ts <- (:[]) <$> tatom <|> parens (sepBy1 texpr comma)
  case ts of
    [t] -> option t $ MLTypeCon ts <$> lowerName
    _   -> MLTypeCon ts <$> lowerName

tatom :: Parser MLType
tatom = MLTypeVar <$> tvar
    <|> MLTypeCon [] <$> lowerName

tvar :: Parser Name
tvar = char '\'' *> lowerName

decl :: Parser MLDecl
decl = reserved "let" >>
         (MLRecDecl <$ reserved "rec"
                    <*> binder
                    <*> (flip (foldr MLFun) <$> many1 binder <* symbol "="
                                            <*> expr)
      <|> MLDecl <$> binder
                 <*> (flip (foldr MLFun) <$> many binder <* symbol "="
                                         <*> expr)
      <|> parens (tuple <$> sepBy binder comma) <* symbol "=" <*> expr)
 where
  tuple [] = MLUnitDecl
  tuple [b] = MLDecl b 
  tuple bs = MLTupleDecl bs

expr' :: Parser MLExpr
expr' = buildExpressionParser table term
 where
  table =
    [ [ unary "-" (Arith Minus), unary "-." (FArith FMinus) ]
    , [ bin "*" (Arith Times), bin "/" (Arith Div)
      , bin "*." (FArith FTimes), bin "/." (FArith FDiv) ]
    , [ bin "+" (Arith Plus), bin "-" (Arith Minus)
      , bin "+." (FArith FPlus), bin "-." (FArith FMinus) ]
    , [ bin "=" (Cmp Eq), bin "<>" (Cmp Neq), bin "<" (Cmp Lt)
      , bin "<=" (Cmp Le), bin ">" (Cmp Gt), bin ">=" (Cmp Ge) ] 
    , [ Infix ((\x y -> MLCon (Raw "::") [x, y]) <$ reservedOp "::") AssocRight ] 
    ]
  unary op t = Prefix ((MLOp t . (:[])) <$ reservedOp op)
  bin op t = Infix (((MLOp t .) . (\x y -> [x, y])) <$ reservedOp op) AssocLeft

expr :: Parser MLExpr
expr = chainr1 expr' ((MLLet . MLUnitDecl) <$ semi)

term :: Parser MLExpr
term = MLIf <$ reserved "if"
            <*> expr' <* reserved "then"
            <*> expr' <* reserved "else"
            <*> expr'
   <|> MLLet <$> decl <* reserved "in"
             <*> expr
   <|> MLMatch <$ reserved "match"
               <*> expr <* reserved "with" <* optional (symbol "|")
               <*> sepEndBy1 alt (symbol "|")
   <|> flip (foldr MLFun) <$ reserved "fun"
                          <*> many1 binder <* symbol "->"
                          <*> expr
   <|> MLCon <$> upperName <*> option [] (untuple <$> atom)
   <|> chainl1 atom (pure MLApply)
 where
  untuple (MLTuple es) = es
  untuple e = [e]

binder :: Parser Binder
binder = try lowerName
     <|> Erased <$ symbol "_"

alt :: Parser MLAlt
alt = MLConCase <$> upperName <*> many binder <* symbol "->" <*> expr
  <|> MLConCase (Raw "::") <$> ((\x y -> [x, y]) <$> binder <* reservedOp "::" <*> binder)
                     <* symbol "->" <*> expr
  <|> MLConCase (Raw "[]") [] <$ symbol "[" <* symbol "]" <* symbol "->" <*> expr
  <|> MLDefaultCase <$ symbol "_" <* symbol "->" <*> expr

atom :: Parser MLExpr
atom = MLVar <$> lowerName
   <|> MLValue <$> constant
   <|> (foldr cons nil) <$> brackets (sepBy expr semi) 
   <|> parens (tuple <$> sepBy expr comma)
 where
  tuple [] = MLValue UnitValue
  tuple [e] = e
  tuple es = MLTuple es
  cons x y = MLCon (Raw "::") [x, y]
  nil = MLCon (Raw "[]") []

constant :: Parser Value
constant = either (IntValue . fromInteger) (FloatValue . double2Float) <$> number 
       <|> BoolValue True <$ reserved "true"
       <|> BoolValue False <$ reserved "false"

-- Lexer

reservedWords :: [String]
reservedWords =
  [ "type", "of", "and", "let", "rec", "in"
  , "if", "then", "else", "match", "with", "fun"
  , "true", "false", "_"
  ]

reservedOps :: [String]
reservedOps =
  [ "=", "<>", "<", ">", "<=", ">=", "::"
  , "+", "-", "*", "/", "+.", "-.", "*.", "/."
  ]

mlDef :: LanguageDef st
mlDef = emptyDef
  { T.commentStart    = "(*"
  , T.commentEnd      = "*)"
  , T.identStart      = letter <|> char '_'
  , T.identLetter     = alphaNum <|> char '_'
  , T.opLetter        = oneOf "+-*/!#$%&\'\"@^~=:;,.<>"
  , T.reservedOpNames = reservedOps
  , T.reservedNames   = reservedWords
  , T.caseSensitive   = True
  }

lexer = T.makeTokenParser mlDef
reserved = T.reserved lexer
reservedOp = T.reservedOp lexer
ident = T.identifier lexer
natural = fromInteger <$> T.natural lexer
symbol = T.symbol lexer
parens = T.parens lexer
brackets = T.brackets lexer
semi = T.semi lexer
comma = T.comma lexer
whiteSpace = T.whiteSpace lexer

upperName = T.lexeme lexer $ (Raw .) . (:) <$> upper <*> many (T.identLetter mlDef)
lowerName = T.lexeme lexer $ try $ do
  name <- (:) <$> T.identStart mlDef <*> many (T.identLetter mlDef)
  if elem name reservedWords
    then unexpected ("reserved word " ++ show name)
    else return $ Raw name

number = try $ fmap fmap sign <*> T.naturalOrFloat lexer
 where
  sign = id <$ char '+'
     <|> negate <$ char '-'
     <|> pure id
