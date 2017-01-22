{
module Orchid.CADM.Parser ( 
    parse
) where

import Orchid.CADM.Lexer
import Orchid.CADM.Syntax

import qualified Data.ByteString.Lazy.Char8 as B

}

%name parseCADMSearchExp
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
  var {Var _ $$}
  int {Int _ $$}

%%

exp  : exp1         { [$1] }
     | exp1 spc exp { $1:$3 }

exp1 : chars       { Word (reverse $1) }
     | '{' exp '}' { SubExp $2 }
     | repeat      { $1 }

repeat : exp1 '[' int ']' { Repeat $1 (Limited $3) (Limited $3) }

chars : char        { [$1] }
      | chars char  { $2:$1 }

char : lit { Literal $1 }
     | wld { parseWildcard $1 }

{
parseWildcard :: Char -> WordElem
parseWildcard '*' = AnyChars
parseWildcard '?' = AnyChar
parseWildcard c   = error ("internal error: unable to parse " ++ [c] ++ " as wildcard")

--parse :: String -> Either String CADMSearchExp
parse s = runAlex s parseCADMSearchExp

parseError :: Token -> Alex a
parseError token = alexError ("parse error at " ++ show token)
}