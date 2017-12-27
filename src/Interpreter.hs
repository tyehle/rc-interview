module Interpreter where

import Parser

data AST = Plus AST AST
         | Sub AST AST
         | ENum Int deriving (Eq, Show)

toAST :: SExp -> AST
toAST (Node [Leaf "+", a, b]) = Plus (toAST a) (toAST b)
toAST (Node [Leaf "-", a, b]) = Sub (toAST a) (toAST b)
toAST (Leaf s) = ENum (read s)

interp :: AST -> Int
interp (Plus a b) = interp a + interp b
interp (Sub a b) = interp a - interp b
interp (ENum n) = n
