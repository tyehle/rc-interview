module Parser where

import Text.Parsec

data SExp = Leaf String | Node [SExp] deriving (Show)


type Parser = Parsec String ()


lexer :: Parser SExp
lexer = inParens undefined


inParens :: Parser a -> Parser a
inParens inner =  between (char '(') (char ')') inner
              <|> between (char '[') (char ']') inner
              <|> between (char '{') (char '}') inner
