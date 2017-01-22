{
{-# LANGUAGE OverloadedStrings #-}
module Orchid.CADM.Lexer (
    Alex
  , Token (..)
  , alexError
  , runAlex
  , lexwrap
) where

import qualified Data.ByteString.Lazy.Char8 as B
}

-- TODO: use regular strings
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
  \{                                      { tok (\p _ -> CBL p) }
  \}                                      { tok (\p _ -> CBR p) }
  \[                                      { tok (\p _ -> SBL p) }
  \]                                      { tok (\p _ -> SBR p) }
  $white* \| $white*                      { tok (\p _ -> Pip p) }

{

tok f (p, _, input, _) len =
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
           | Pip AlexPosn
           | EOF
  deriving (Eq,Show)

alexEOF = return EOF

lexwrap = (alexMonadScan >>=)
}

