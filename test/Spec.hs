import Test.Tasty

import ParserSpec

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ parserTests
  ]
