module Orchid.CADM.Syntax 
  ( CADMSearchExp (..)
  , WordElem (..)
  , RepeatBound (..)
  ) where

data CADMSearchExp = Repeat CADMSearchExp RepeatBound RepeatBound
                   | Word [WordElem]
                   | Variable String
                   | SubExp [CADMSearchExp]
                   | Alternative CADMSearchExp CADMSearchExp
  deriving (Eq, Show)

data WordElem = Literal Char
              | AnyChars
              | AnyChar
  deriving (Eq, Show)

data RepeatBound = Limited Int
                 | Unlimited
  deriving (Eq, Show)
