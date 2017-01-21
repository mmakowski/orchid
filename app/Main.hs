{-# LANGUAGE OverloadedStrings #-}
module Main where

import Orchid.Lexer

main :: IO ()
main = case scanner "du\\??-pa[13]" of
          Left err -> error err
          Right toks -> print toks
