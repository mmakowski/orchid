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
%error {parseError}

%token
  spc {Spc _}
  lit {Lit _ $$}
  wld {Wld _ $$}
  '[' {SBL _}
  ']' {SBR _}
  var {Var _ $$}
  int {Int _ $$}

%%

exp  : exp1 spc exp { $1:$3 }
     | exp1         { [$1] }

exp1 : atoms        { Word (reverse $1) }

atoms : atom        { [$1] }
      | atoms atom  { $2:$1 }

atom : lit { Literal $1 }
     | wld { parseWildcard $1 }

{
data CadmSearchExp = Repeat CadmSearchExp RepeatBound RepeatBound
                   | Word [WordElem]
  deriving (Eq, Show)

data WordElem = Literal Char
              | AnyChars
              | AnyChar
  deriving (Eq, Show)

data RepeatBound = Limited Int
                 | Unlimited
  deriving (Eq, Show)

parseWildcard :: Char -> WordElem
parseWildcard '*' = AnyChars
parseWildcard '?' = AnyChar
parseWildcard c   = error ("internal error: unable to parse " ++ [c] ++ " as wildcard")

--parse :: String -> Either String CadmSearchExp
parse s = runAlex s parseCadmSearchExp

parseError :: Token -> Alex a
parseError token = alexError ("parse error at " ++ show token)
}