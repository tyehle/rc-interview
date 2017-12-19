module Parser where

import Text.Parsec
import Data.Char (isSpace)
import Data.Bifunctor (first)


data SExp = Leaf String | Node [SExp] deriving (Show, Eq)


parseSExp :: String -> Either String SExp
parseSExp = first show . parse sexp ""


type Parser = Parsec String ()


sexp :: Parser SExp
sexp = node <|> leaf


node :: Parser SExp
node = inParens (Node <$> sepBy1 sexp (many1 space))


leaf :: Parser SExp
leaf = Leaf <$> many1 (satisfy isAllowed)
  where
    isAllowed c = not (isSpace c || c `elem` "([{}])")


inParens :: Parser a -> Parser a
inParens inner =  between (char '(') (char ')') eatSpaces
              <|> between (char '[') (char ']') eatSpaces
              <|> between (char '{') (char '}') eatSpaces
  where
    eatSpaces = spaces *> inner <* spaces
