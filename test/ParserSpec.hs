module ParserSpec (parserTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Parser
import Text.Parsec

testParse :: Parsec String () a -> String -> Maybe a
testParse parser = either (const Nothing) Just . parse parser "testInput"

parserTests :: TestTree
parserTests = testGroup "Parser Tests"
  [ leafTests
  , nodeTests
  ]

leafTests :: TestTree
leafTests = testGroup "Leaf Tests"
  [ testCase "1" $ testParse leaf "asdf)" @?= Just (Leaf "asdf")
  , testCase "2" $ testParse leaf "asdf " @?= Just (Leaf "asdf")
  , testCase "3" $ testParse leaf "}" @?= Nothing
  ]

nodeTests :: TestTree
nodeTests = testGroup "Node Tests"
  [ testCase "1" $ testParse node "(asdf)" @?= Just (Node [Leaf "asdf"])
  , testCase "2" $ testParse node "[asdf]" @?= Just (Node [Leaf "asdf"])
  , testCase "3" $ testParse node "{asdf}" @?= Just (Node [Leaf "asdf"])
  , testCase "4" $ testParse node "(asdf" @?= Nothing
  , testCase "5" $ testParse node "(asdf]" @?= Nothing
  , testCase "6" $ testParse node "(asdf))" @?= Just (Node [Leaf "asdf"])
  , testCase "7" $ testParse node "( asdf )" @?= Just (Node [Leaf "asdf"])
  , testCase "8" $ testParse node "(a b)" @?= Just (Node [Leaf "a", Leaf "b"])
  , testCase "9" $ testParse node "((a) [{b}])" @?=
      Just (Node [Node [Leaf "a"], Node [Node [Leaf "b"]]])
  , testCase "10" $ testParse node "( asdf )" @?= Just (Node [Leaf "asdf"])
  ]
