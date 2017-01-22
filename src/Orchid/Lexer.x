{
{-# LANGUAGE OverloadedStrings #-}
module Orchid.Lexer (
    Alex
  , Token (..)
  , alexError
  , runAlex
  , lexwrap
) where

import qualified Data.ByteString.Lazy.Char8 as B
}

%wrapper "monad-bytestring"

$digit = 0-9
$alpha = [a-zA-Z]
$special = [\{\}\|\[\]\%\?\*\\]
$wildcard = [\?\*]

tokens :-

  \% (\+|\-)? $alpha [$alpha $digit]* \%  { tok (\p s -> Var p (B.unpack s)) }
  $white+                                 { tok (\p _ -> Spc p) }
  $digit+                                 { tok (\p s -> Int p (read (B.unpack s))) }
  [$alpha $digit [\-]] | \\ $special      { tok (\p s -> Lit p (last (B.unpack s))) }
  $wildcard                               { tok (\p s -> Wld p (head (B.unpack s))) }
  \{                                      { tok (\p s -> CBL p) }
  \}                                      { tok (\p s -> CBR p) }
  \[                                      { tok (\p s -> SBL p) }
  \]                                      { tok (\p s -> SBR p) }

{

tok f (p,_,input,_) len =
  return (f p (B.take (fromIntegral len) input))

data Token = Spc AlexPosn
           | Lit AlexPosn Char
           | Wld AlexPosn Char
           | CBL AlexPosn
           | CBR AlexPosn
           | SBL AlexPosn
           | SBR AlexPosn
           | Var AlexPosn String
           | Int AlexPosn Int
           | EOF
  deriving (Eq,Show)

alexEOF = return EOF

lexwrap = (alexMonadScan >>=)
}

