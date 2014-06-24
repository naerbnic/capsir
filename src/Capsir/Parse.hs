{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Capsir.Parse
  (parseCapsir) where

import Capsir
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token

-- Ambiguous Parser Combinators

langDef :: LanguageDef st
langDef = LanguageDef
    { commentStart = "/*"
    , commentEnd = "*/"
    , commentLine = "//"
    , nestedComments = True
    , identStart = letter <|> char '_'
    , identLetter = alphaNum <|> char '_'
    , opStart = oneOf "=>?@|"
    , opLetter = oneOf ">"
    , reservedNames = [ "let", "in", "exit", "let" ]
    , reservedOpNames = [ ">>", "=>", "?", "|" ]
    , caseSensitive = True
    }

lexer :: TokenParser st
lexer = makeTokenParser langDef

parseParamList :: Parser a -> Parser [a]
parseParamList p = parens lexer $ commaSep lexer p

ident :: Parser String
ident = identifier lexer

parseLiteral :: Parser Literal
parseLiteral = do
    _ <- char '@'
    name <- ident
    params <- parseParamList parseLitParam
    return $ Literal name params

parseCont :: Parser Cont
parseCont = do
    args <- parseParamList ident
    reservedOp lexer "=>"
    expr <- parseExpr
    return $ Cont args expr

parseLitParam :: Parser LitParam
parseLitParam = try (
      -- Recursive Literal
      fmap LitParamLit parseLiteral
    ) <|> try (
      -- String
      fmap LitParamString $ stringLiteral lexer
    ) <|> try (
      -- Float
      fmap LitParamFloat $ float lexer
    ) <|> try (
      -- Int
      fmap (LitParamInt . fromInteger) $ integer lexer
    ) <?> "Unable to parse literal parameter"

parseValue :: Parser Value
parseValue = try (do
    fmap ContValue parseCont
  ) <|> try (
    fmap LitValue parseLiteral
  ) <|> try (
    fmap VarValue ident
  ) <?> "Unable to parse Value"

parseLetTerm :: Parser (String, Cont)
parseLetTerm = do
  name <- ident
  cont <- parseCont
  _ <- semi lexer
  return (name, cont)

parseExpr :: Parser CpsExpr
parseExpr = try (do
      -- Parses an apply expression
      values <- parseParamList parseValue
      reservedOp lexer ">>"
      contValue <- parseValue
      return $ Apply values contValue
    ) <|> try (do
      -- Parses a singleton instruction
      name <- ident
      values <- parseParamList parseValue
      reservedOp lexer ">>"
      contValue <- parseValue
      return $ Inst name values [contValue]
    ) <|> try (do
      -- Parses a branch instruction
      name <- ident
      values <- parseParamList parseValue
      reservedOp lexer "?"
      contValues <- braces lexer $ sepBy1 parseValue $ reservedOp lexer "|"
      return $ Inst name values contValues
    ) <|> try (do
      -- Parses an exit expression
      reserved lexer "exit"
      value <- parseValue
      return $ Exit value
    ) <|> try (do
      reserved lexer "fix"
      pairs <- braces lexer $ do
          many1 parseLetTerm
      expr <- parseExpr
      return $ Fix pairs expr
    ) <?> "Unable to parse CPS Expr"
    
parseCapsir :: String -> Either ParseError CpsExpr
parseCapsir source =
    let parser = do
            whiteSpace lexer
            expr <- parseExpr
            eof
            return expr
    in parse parser "Unknown" source

