{
module Orchid.Lexer (Token(..), P, evalP, lexer) where

import Control.Monad.State
import Control.Monad.Error
import Data.Word
}

%wrapper "monad"

tokens :-
    $white+      ;
    true      {mkT TTrue}
    false   {mkT TFalse}
    0       {mkT TZero}
    succ    {mkT TSucc}
    pred    {mkT TPred}
    if     {mkT TIf}
    then     {mkT TThen}
    else     {mkT TElse}
    iszero   {mkT TIsZero}

{
data Token = 
     TTrue
     | TFalse
     | TZero
     | TSucc
     | TPred
     | TIf
     | TThen
     | TElse
     | TIsZero
     | TEOF
     deriving (Eq, Show)

type P a = StateT AlexInput (Either String) a

evalP :: P a -> AlexInput -> Either String a
evalP = evalStateT

alexEOF :: Alex Token
alexEOF = return TEOF


mkT :: Token -> AlexInput -> Int -> Alex Token -- TODO: record token positions
mkT c (p, _, _, str) len = let t = take len str
                           in return c
{-
                           in case c of
                                LInteger -> return (IntegerNum ((read t) :: Integer) p)
                                LBoolean -> return (BooleanVal (if t == "true"
                                                                   then True
                                                                   else False
                                                               ) p)
                                LString -> return (StringTxt (take (length t - 2) (drop 1 t)) p)
                                LIdentifier -> return (Identifier t p)
                                LSection -> return (SectionHeader (take (length t - 2) (drop 1 t)) p)
                                LAssign -> return (Assignment p)
                                LEndAssign -> return (EndAssignment p)
-}

lexer :: (Token -> Alex a) -> Alex a
lexer cont = alexMonadScan >>= cont
}