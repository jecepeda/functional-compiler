{
module Grammar where
import Tokens
}

%name parseTokenss
%tokentype { Token }
%error { parseError }

$whiteSpace = [\ ]

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
    'else'      { TokenElse }
    'endif'     { TokenEndIf }
    'while'     { TokenWhile }
    'endwhile'  { TokenEndWhile }
    'print'     { TokenPrint }
    '[\ ]'      { TokenWhiteSpace }

%left '+' '-'

%%

Prog : Exp Prog              { $1 : $2 } 
     | Exp                   { [$1] }

Exp : var sym '=' IntOp nl     { Assign $2 $4 }
    | IntOp nl                 { Tok $1 }
    | sym nl                   { Sym $1 }
    | IfCond nl                { Tok $1 }
    | WhileCond nl             { Tok $1 }
    | Print nl                 { Sym $1 }

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

IfCond : if Cond nl Exp nl endif            { [$4] }

ElseCond : if Cond nl else Exp nl endif     { [$5] }

WhileCond : while Cond nl Exp nl endwhile   { [$4] }

Print : Exp '[\ ]' Print                    { [$1] }
          


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
