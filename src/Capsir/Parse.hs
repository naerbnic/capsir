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
    , reservedNames = [ "let", "in", "exit" ]
    , reservedOpNames = [ ">>", "=>", "?", "|" ]
    , caseSensitive = True
    }

lexer :: TokenParser st
lexer = makeTokenParser langDef

parseParamList :: Parser a -> Parser [a]
parseParamList p = parens lexer $ commaSep lexer p

ident :: Parser String
ident = identifier lexer

parseValue :: Parser Value
parseValue = try (do
    args <- parseParamList ident
    reservedOp lexer "=>"
    expr <- parseExpr
    return $ ContValue $ Cont args expr
  ) <|> try (do
    _ <- char '@'
    name <- ident
    _ <- parseParamList(char '_') -- Clearly, add a rule to parse literal params
    return $ LitValue $ Literal name []
  ) <|> try (fmap VarValue ident) <?> "Unable to parse Value"


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
    ) <?> "Unable to parse CPS Expr"
    
parseCapsir :: String -> Either ParseError CpsExpr
parseCapsir source =
    let parser = do
            whiteSpace lexer
            expr <- parseExpr
            eof
            return expr
    in parse parser "Unknown" source

