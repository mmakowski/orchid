{
{-# LANGUAGE OverloadedStrings #-}
module Orchid.Lexer (scanner) where
import System.Exit
import qualified Data.ByteString.Lazy.Char8 as B
}

%wrapper "monad-bytestring"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$special = [\{\}\|\[\]\%\?\*\\]
$wildcard = [\?\*]

tokens :-

  $white+               ;
  $digit+               { tok (\p s -> Int p (read (B.unpack s))) }
  ([$alpha $digit $wildcard [\-]] | \\ $special)+  { tok (\p s -> Word p (B.unpack s)) }
  [\[\]\,]              { tok (\p s -> Sym p (head (B.unpack s))) }

{
-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:
tok f (p,_,input,_) len = return (f p (B.take (fromIntegral len) input))

data Token =
    Word AlexPosn String |
    Sym AlexPosn Char   |
    Var AlexPosn String     |
    Int AlexPosn Int    |
    Err AlexPosn            |
    EOF
        deriving (Eq,Show)

alexEOF = return EOF

scanner str = runAlex str $ do
  let loop = do tok <- alexMonadScan
                if tok == EOF
                then return [tok]
                else do toks <- loop
                        return (tok:toks)
  loop
}