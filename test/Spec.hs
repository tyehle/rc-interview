import Test.Tasty

import InterpreterSpec
import ParserSpec

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ parserTests
  , interpreterTests
  ]
