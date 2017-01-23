{
module Orchid.CADM.Parser ( 
    parse
) where

import Data.Char (toLower)

import Orchid.CADM.Lexer
import Orchid.CADM.Syntax

import qualified Data.ByteString.Lazy.Char8 as B

}

%name parseCADMSearchExps
%tokentype { Token }
%lexer {lexwrap} {EOF}
%monad {Alex}
%error {parseError}

%token
  spc {Spc _}
  lit {Lit _ $$}
  wld {Wld _ $$}
  '{' {CBL _}
  '}' {CBR _}
  '[' {SBL _}
  ']' {SBR _}
  '|' {Pip _}
  var {Var _ $$}
  int {Int _ $$}

%%

exps : exp          { [$1] }
     | exp spc exps { $1:$3 }

exp  : chars         { Word (reverse $1) }
     | '{' exps '}'  { SubExp $2 }
     | repeat        { $1 }
     | exp '|' exp   { Alternative $1 $3 }
     | var           { Variable (map toLower $1) }

repeat : exp '[' int ']' { Repeat $1 (Limited $3) (Limited $3) }
       | exp '[' wld ']' { parseRepeatChar $3 $1 }

chars : char        { [$1] }
      | chars char  { $2:$1 }

char : lit { Literal $1 }
     | wld { parseWildcard $1 }

{
parseWildcard :: Char -> WordElem
parseWildcard '*' = AnyChars
parseWildcard '?' = AnyChar
parseWildcard c   = error ("internal error: unable to parse " ++ [c] ++ " as wildcard")

parseRepeatChar :: Char -> CADMSearchExp -> CADMSearchExp
parseRepeatChar '+' exp = Repeat exp (Limited 1) Unlimited
parseRepeatChar '*' exp = Repeat exp (Limited 0) Unlimited
parseRepeatChar '?' exp = Repeat exp (Limited 0) (Limited 1)

--parse :: String -> Either String [CADMSearchExp]
parse s = runAlex s parseCADMSearchExps

parseError :: Token -> Alex a
parseError token = alexError ("parse error at " ++ show token)
}