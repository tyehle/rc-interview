module InterpreterSpec (interpreterTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Parser
import Interpreter

parse :: String -> SExp
parse s = either undefined id $ parseSExp s

interpreterTests = testGroup "Interpreter Tests"
  [ testCase "0" $ interp (toAST (parse "(+ 1 2)")) @?= 3
  , testCase "1" $ interp (toAST (parse "(+ 1 (+ 2 3))")) @?= 6
  , testCase "2" $ interp (toAST (parse "(- 2 5)")) @?= -3
  , testCase "3" $ interp (toAST (parse "(- 2 (- 7 2))")) @?= -3
  ]
