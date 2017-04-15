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
           deriving (Eq,Show)

scanTokens = alexScanTokens

}