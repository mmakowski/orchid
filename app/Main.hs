{-# LANGUAGE OverloadedStrings #-}
module Main where

import Orchid.Lexer

main :: IO ()
main = case scanner "let a = 13 in a" of
          Left err -> error err
          Right toks -> print toks
