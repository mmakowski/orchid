{
module Orchid.Parser ( 
    parse
  , CadmSearchExp (..)
  , WordElem (..)
  , RepeatBound (..)
) where

import Orchid.Lexer

import qualified Data.ByteString.Lazy.Char8 as B

}

%name parseCadmSearchExp
%tokentype { Token }
%lexer {lexwrap} {EOF}
%monad {Alex}

%token
  spc {Spc _}
  lit {Lit _ $$}
  wld {Wld _ $$}
  '[' {SBL _}
  ']' {SBR _}
  var {Var _ $$}
  int {Int _ $$}

%%

Exp  : Exp1 spc Exp { $1:$3 }
     | Exp1         { [$1] }

Exp1 : lit          { Word [Literal $1] }

{
data CadmSearchExp = Exp [CadmSearchExp]
                   | Repeat CadmSearchExp RepeatBound RepeatBound
                   | Word [WordElem]
  deriving (Eq, Show)

data WordElem = Literal Char
              | AnyChars
              | AnyChar
  deriving (Eq, Show)

data RepeatBound = Limited Int
                 | Unlimited
  deriving (Eq, Show)

--parse :: String -> Either String CadmSearchExp
parse s = runAlex s parseCadmSearchExp

happyError :: Alex a
happyError = alexError "TODO: good error message"
}