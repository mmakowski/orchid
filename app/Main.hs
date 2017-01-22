{-# LANGUAGE OverloadedStrings #-}
module Main where

import Orchid.CADM.Parser (parse)

main :: IO ()
main =
  print $ parse "noga | {du?a[2] d*a\\?}[4] geba %false%"
