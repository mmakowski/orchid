{-# LANGUAGE OverloadedStrings #-}
module Main where

import Orchid.Lexer
import Orchid.Parser (parse)

main :: IO ()
main =
  print $ parse "d u p a"
