module Main where

import Lib
import Grammar
import Tokens

main = do
  content <- readFile "app/example"
  let tokens = scanTokens content
  let structure = parseTokenss tokens
  eval structure emptyDataStore