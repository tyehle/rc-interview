module Main where

import Parser (parseSExp, SExp(..))
import Data.List (intercalate)
import Data.Char (isSpace)

main :: IO ()
main = do
  input <- getContents
  let trimmed = dropWhile isSpace input
  either print (putStrLn . ("\n"++) . prettySExp) . parseSExp $ trimmed

prettySExp :: SExp -> String
prettySExp (Leaf e) = e
prettySExp (Node es) = "(" ++ unwords (map prettySExp es) ++ ")"
