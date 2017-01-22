{-# LANGUAGE OverloadedStrings #-}
module Main where

import Orchid.Parser (parse)

main :: IO ()
main =
  print $ parse "du?a\\? d*a"
