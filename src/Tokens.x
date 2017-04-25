{
module Tokens where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$whiteSpace = [\ ]

tokens :-
  $whiteSpace+                   ;
  [\n \;]+                      { \s -> TokenNewLine}
  var                           { \s -> TokenVar }
  $digit+                       { \s -> TokenInt (read s) }
  \=                            { \s -> TokenAssign }
  \+                            { \s -> TokenPlus }
  \-                            { \s -> TokenMinus }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }
  \||                           { \s -> TokenOr }
  \&&                           { \s -> TokenAnd}
  \<                            { \s -> TokenLess}
  \>                            { \s -> TokenGreater}
  \<=                           { \s -> TokenLessEqual}
  \>=                           { \s -> TokenGreaterEqual}
  \if                           { \s -> TokenIf}
  \endif                        { \s -> TokenEndIf}
  \while                        { \s -> TokenWhile}
  \endwhile                     { \s -> TokenEndWhile}
  \print                        { \s -> TokenPrint}


{

-- The token type:
data Token = TokenVar
           | TokenInt Int
           | TokenSym String
           | TokenAssign
           | TokenPlus
           | TokenMinus
           | TokenNewLine
           | SpaceToken String
           | TokenOr
           | TokenAnd
           | TokenLess
           | TokenGreater
           | TokenLessEqual
           | TokenGreaterEqual
           | TokenIf
           | TokenEndIf
           | TokenWhile
           | TokenEndWhile
           | TokenPrint
           deriving (Eq,Show)

scanTokens = alexScanTokens

}