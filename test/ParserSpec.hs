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
  , leafTests
  , nodeTests
  ]

parenTests :: TestTree
parenTests = testGroup "Paren Tests"
  [ testCase "1" $ testParen "(asdf)" @?= Just "asdf"
  , testCase "2" $ testParen "[asdf]" @?= Just "asdf"
  , testCase "3" $ testParen "{asdf}" @?= Just "asdf"
  , testCase "4" $ testParen "(asdf" @?= Nothing
  , testCase "5" $ testParen "(asdf]" @?= Nothing
  , testCase "6" $ testParen "(asdf))" @?= Just "asdf"
  , testCase "7" $ testParen "( asdf )" @?= Just "asdf"
  ]
  where
    testParen = testParse (inParens (string "asdf"))

leafTests :: TestTree
leafTests = testGroup "Leaf Tests"
  [ testCase "1" $ testParse leaf "asdf)" @?= Just (Leaf "asdf")
  , testCase "2" $ testParse leaf "asdf " @?= Just (Leaf "asdf")
  , testCase "3" $ testParse leaf "}" @?= Nothing
  ]

nodeTests :: TestTree
nodeTests = testGroup "Node Tests"
  [ testCase "1" $ testParse node "(a)" @?= Just (Node [Leaf "a"])
  , testCase "2" $ testParse node "(a b)" @?= Just (Node [Leaf "a", Leaf "b"])
  , testCase "3" $ testParse node "((a) [{b}])" @?=
      Just (Node [Node [Leaf "a"], Node [Node [Leaf "b"]]])
  ]
