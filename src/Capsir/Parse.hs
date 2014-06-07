module Capsir.Parse where

import Capsir
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token

-- Ambiguous Parser Combinators

newtype StreamSet s = StreamSet { getStreams:: [s] }

langDef :: LanguageDef st
langDef = LanguageDef
    { commentStart = "/*"
    , commentEnd = "*/"
    , commentLine = "//"
    , nestedComments = True
    , identStart = letter <|> char '_'
    , identLetter = alphaNum <|> char '_'
    , opStart = oneOf "=>?@"
    , opLetter = oneOf ">"
    , reservedNames = [ "let", "in" ]
    , reservedOpNames = [ ">>", "=>", "?" ]
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
parseExpr = undefined
    
parse :: String -> Either [ParseError] CpsExpr
parse = undefined
