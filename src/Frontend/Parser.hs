module Frontend.Parser
  ( parseMLProgram
  , parseMLExpr
  ) where

import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad.Error
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as T
import Data.Either
import Data.List

import Frontend.AST
import Frontend.Dianostic
import Internal

runParser' :: Parser a -> String -> Frontend a
runParser' p input =
  case parse (whiteSpace *> p <* eof) "(input)" input of
    Left e -> throwError $ strMsg $ show e
    Right a -> return a

parseMLProgram :: String -> Frontend MLProgram
parseMLProgram = runParser' program

parseMLExpr :: String -> Frontend MLExpr
parseMLExpr = runParser' expr

-- Parser

program :: Parser MLProgram
program = fmap partitionEithers $ sepEndBy1 decl' $ optional $ symbol ";;"
 where
  decl' = Left <$> tdecl <|> Right <$> decl

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
                                         <*> expr))

expr :: Parser MLExpr
expr = buildExpressionParser table term
 where
  table =
    [ [unary "-" Minus, unary "-." FMinus]
    , [bin "*" Times, bin "/" Div, bin "*." FTimes, bin "/." FDiv]
    , [bin "+" Plus, bin "-" Minus, bin "+." FPlus, bin "-." FMinus]
    , [bin "=" Eq, bin "<" Lt, bin "<=" Le, bin ">" Gt, bin ">=" Ge] 
    , [Infix ((\x y -> MLCon "::" [x, y]) <$ reservedOp "::") AssocRight] 
    ]
  unary op t = Prefix ((MLOp t . (:[])) <$ reservedOp op)
  bin op t = Infix (((MLOp t .) . (\x y -> [x, y])) <$ reservedOp op) AssocLeft

term :: Parser MLExpr
term = MLIf <$ reserved "if"
            <*> expr <* reserved "then"
            <*> expr <* reserved "else"
            <*> expr
   <|> MLLet <$> decl <* reserved "in"
             <*> expr
   <|> MLMatch <$ reserved "match"
               <*> expr <* reserved "with"
               <*> sepEndBy1 alt (symbol "|")
   <|> flip (foldr MLFun) <$ reserved "fun"
                          <*> many1 binder <* symbol "->"
                          <*> expr
   <|> MLCon <$> upperName <*> option [] (untuple <$> atom)
   <|> chainl1 atom (pure MLApply)
 where
  untuple (MLTuple exprs) = exprs
  untuple expr = [expr]

binder :: Parser MLBinder
binder = try (Just <$> lowerName)
     <|> Nothing <$ symbol "_"

alt :: Parser MLAlt
alt = MLConCase <$> upperName <*> many binder <* symbol "->" <*> expr
  <|> MLConCase "::" <$> ((\x y -> [x, y]) <$> binder <* reservedOp "::" <*> binder)
                     <* symbol "->" <*> expr
  <|> MLConCase "[]" [] <$ symbol "[" <* symbol "]" <* symbol "->" <*> expr
  <|> MLDefaultCase <$ symbol "_" <* symbol "->" <*> expr

atom :: Parser MLExpr
atom = MLVar <$> lowerName
   <|> MLValue <$> constant
   <|> (foldr cons nil) <$> brackets (sepBy expr semi) 
   <|> parens (tuple <$> sepBy expr comma)
 where
  tuple [] = MLValue UnitValue
  tuple [expr] = expr
  tuple exprs = MLTuple exprs
  cons x y = MLCon "::" [x, y]
  nil = MLCon "[]" []

constant :: Parser Value
constant = either (IntValue . fromInteger) FloatValue <$> number 
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

upperName = T.lexeme lexer $ (:) <$> upper <*> many (T.identLetter mlDef)
lowerName = T.lexeme lexer $ try $ do
  name <- (:) <$> (lower <|> char '_') <*> many (T.identLetter mlDef)
  if elem name reservedWords
    then unexpected ("reserved word " ++ show name)
    else return name

number = try $ fmap fmap sign <*> T.naturalOrFloat lexer
 where
  sign = id <$ char '+'
     <|> negate <$ char '-'
     <|> pure id
