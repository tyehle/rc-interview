module Main where

main :: IO ()
main = do
  allInput <- getContents
  putStrLn . head . lines $ allInput
