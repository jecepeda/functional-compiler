{
module Tokens where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$whiteSpace = [\ ]

tokens :-
  $whiteSpace+                  ;
  \t+                           ;
  [\n \;]+                      { \s -> TokenNewLine}
  var                           { \s -> TokenVar }
  if                            { \s -> TokenIf }
  else                          { \s -> TokenElse }
  endif                         { \s -> TokenEndIf}
  while                         { \s -> TokenWhile}
  endwhile                      { \s -> TokenEndWhile}
  print                         { \s -> TokenPrint}
  \<                            { \s -> TokenLess }
  \>                            { \s -> TokenGreater }
  \<=                           { \s -> TokenLessEqual }
  \>=                           { \s -> TokenGreaterEqual }
  \=                            { \s -> TokenAssign }
  \+                            { \s -> TokenPlus }
  \-                            { \s -> TokenMinus }
  \*                            { \s -> TokenMultiply }
  \/                            { \s -> TokenDivide }
  $digit+                       { \s -> TokenInt (read s) }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }

{

-- The token type:
data Token = TokenVar
           | TokenInt Int
           | TokenSym String
           | TokenAssign
           | TokenPlus
           | TokenMinus
           | TokenMultiply
           | TokenDivide
           | TokenNewLine
           | SpaceToken String
           | TokenOr
           | TokenAnd
           | TokenLess
           | TokenGreater
           | TokenLessEqual
           | TokenGreaterEqual
           | TokenIf
           | TokenElse
           | TokenEndIf
           | TokenWhile
           | TokenEndWhile
           | TokenPrint
           deriving (Eq,Show)

scanTokens = alexScanTokens

}