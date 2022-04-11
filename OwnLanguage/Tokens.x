{ 
module Tokens where
import System.Environment

}

%wrapper "posn" 
$digit = 0-9     
$alpha = [a-zA-Z\.\:\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/\<\>]
$filealpha = [a-zA-Z]

tokens :-
  $white+                                      ;
  "--".*                                       ;
  READ                                         { \p s -> TokenRead p }
  WHERE                                        { \p s -> TokenWhere p }
  SUBJ                                         { \p s -> TokenSubj p }
  PRED                                         { \p s -> TokenPred p }
  OBJ                                          { \p s -> TokenObj p }
  AND                                          { \p s -> TokenAnd p }
  OR                                           { \p s -> TokenOr p }
  IF                                           { \p s -> TokenIf p }
  THEN                                         { \p s -> TokenThen p }
  ELSE                                         { \p s -> TokenElse p }
  END                                          { \p s -> TokenEnd p }
  DROP                                         { \p s -> TokenDrop p }
  CHANGE                                       { \p s -> TokenChange p }
  INSERT                                       { \p s -> TokenInsert p }
  \.                                           { \p s -> TokenDot p }
  \<                                           { \p s -> TokenLBrack p }
  \>                                           { \p s -> TokenRBrack p }
  \=                                           { \p s -> TokenEq p }
  \(                                           { \p s -> TokenLParen p }
  \)                                           { \p s -> TokenRParen p }
  \+                                           { \p s -> TokenPlus p }
  \-                                           { \p s -> TokenMinus p }
  True                                         { \p b -> TokenBool p (read b) }
  False                                        { \p b -> TokenBool p (read b) }
  $digit+                                      { \p n -> TokenInt p (read n) }
  \" $alpha [$digit $alpha]* \"                { \p s -> TokenString p s }
  $filealpha [$digit $filealpha]*              { \p s -> TokenFile p s }

{
-- Each action has type :: AlexPosn -> String -> Token

-- The token type:
data Token =
  TokenRead AlexPosn          |
  TokenWhere AlexPosn         |
  TokenSubj AlexPosn          |
  TokenPred AlexPosn          |
  TokenObj AlexPosn           |
  TokenAnd AlexPosn           |
  TokenOr AlexPosn            |
  TokenIf AlexPosn            |
  TokenThen AlexPosn          |
  TokenElse AlexPosn          |
  TokenEnd AlexPosn           |
  TokenDrop AlexPosn          |
  TokenChange AlexPosn        |
  TokenInsert AlexPosn        |
  TokenDot AlexPosn           |
  TokenLBrack AlexPosn        |
  TokenRBrack AlexPosn        |
  TokenEq AlexPosn            |
  TokenLParen AlexPosn        |
  TokenRParen AlexPosn        |
  TokenPlus AlexPosn          |
  TokenMinus AlexPosn         |
  TokenFile AlexPosn String   |
  TokenString AlexPosn String |
  TokenBool AlexPosn Bool     |
  TokenInt AlexPosn Int
  deriving (Eq,Show)

tokenPosn :: Token -> String
tokenPosn (TokenRead (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWhere (AlexPn a l c)) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenSubj (AlexPn a l c)) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenPred (AlexPn a l c)) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenObj (AlexPn a l c)) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenAnd (AlexPn a l c)) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenOr (AlexPn a l c)) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenIf (AlexPn a l c)) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenThen (AlexPn a l c)) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenElse (AlexPn a l c)) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenEnd (AlexPn a l c)) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenDrop (AlexPn a l c)) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenChange (AlexPn a l c)) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenInsert (AlexPn a l c)) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenDot (AlexPn a l c)) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenLBrack (AlexPn a l c)) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenRBrack (AlexPn a l c)) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenEq (AlexPn a l c)) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenLParen (AlexPn a l c)) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c)) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenPlus (AlexPn a l c)) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenMinus (AlexPn a l c)) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenFile (AlexPn a l c) s) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenString (AlexPn a l c) s) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenBool (AlexPn a l c) b) = show (l) ++ ":" ++ show(c)
tokenPosn (TokenInt (AlexPn a l c) n) = show (l) ++ ":" ++ show(c)

}
