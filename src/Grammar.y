{
module Grammar where
import Tokens
}

%name parseTokenss
%tokentype { Token }
%error { parseError }

%token
    nl          { TokenNewLine }
    int         { TokenInt $$ }
    var         { TokenVar}
    sym         { TokenSym $$}
    '='         { TokenAssign }
    '+'         { TokenPlus }
    '-'         { TokenMinus }
    '||'        { TokenOr }
    '&&'        { TokenAnd }
    '<'         { TokenLess }
    '>'         { TokenGreater }
    '<='        { TokenLessEqual }
    '>='        { TokenGreaterEqual}
    'if'        { TokenIf }
    'endif'     { TokenEndIf }
    'while'     { TokenWhile }
    'endwhile'  { TokenEndWhile }

%left '+' '-'

%%

Prog : Exp Prog              { $1 : $2} 
     | Exp                   { [$1] }

Exp : var sym '=' IntOp nl     { Assign $2 $4 }
    | IntOp nl                 { Tok $1 }
    | sym nl                   { Sym $1 }

IntOp : int                  { Int $1 }
    | int '+' IntOp          { Plus $1 $3 } 
    | int '-' IntOp          { Minus $1 $3 }
    | int '*' IntOp          { Multiply $1 $3 }
    | int '/' IntOp          { Divide $1 $3 }

Cond : intOp '||' Cond       { Or $1 $3 }
     | intOp '&&' Cond       { And $1 $3 }
     | intOp '<' Cond        { Less $1 $3 }
     | intOp '>' Cond        { Greater $1 $3 }
     | intOp '<=' Cond       { LessEqual $1 $3 }
     | intOp '>=' Cond       { GreaterEqual $1 $3 }

IfCond : if Cond nl Exp nl endif
       | if Cond nl IntOp nl endif

WhileCond : while Cond nl Exp nl endwhile
          | if Cond nl IntOp nl endwhile


{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp = Assign String IntOp
         | Tok IntOp
         | Sym String
         deriving (Eq, Show)

data IntOp = Int Int
           | Plus Int IntOp
           | Minus Int IntOp
           deriving (Eq, Show)

}
