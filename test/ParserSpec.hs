module ParserSpec (parserTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Parser
import Text.Parsec

testParse :: Parsec String () a -> String -> Maybe a
testParse parser = either (const Nothing) Just . parse parser "testInput"

parserTests :: TestTree
parserTests = testGroup "Parser Tests"
  [ parenTests
  ]

parenTests :: TestTree
parenTests = testGroup "Paren Tests"
  [ testCase "1" $ testParen "(asdf)" @?= Just "asdf"
  , testCase "2" $ testParen "[asdf]" @?= Just "asdf"
  , testCase "3" $ testParen "{asdf}" @?= Just "asdf"
  , testCase "4" $ testParen "(asdf" @?= Nothing
  , testCase "5" $ testParen "(asdf]" @?= Nothing
  , testCase "6" $ testParen "(asdf))" @?= Just "asdf"
  ]
  where
    testParen = testParse (inParens (string "asdf"))
