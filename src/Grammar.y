{
module Grammar where
import Tokens
}

%name parseTokenss
%tokentype { Token }
%error { parseError }

%token
    nl  { TokenNewLine }
    int { TokenInt $$ }
    var { TokenVar}
    sym { TokenSym $$}
    '=' { TokenAssign }
    '+' { TokenPlus }
    '-' { TokenMinus }

%left '+' '-'

%%

Prog : Exp Prog              { $1 : $2} 
     | Exp                   { [$1] }

Exp : var sym '=' IntOp nl     { Assign $2 $4 }
    | IntOp nl                 { Tok $1 }
    | sym nl                   { Sym $1 }

IntOp : int                  { Int $1 }
    | int '+' IntOp          { Plus $1 $3}
    | int '-' IntOp          { Minus $1 $3}

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp = Assign String IntOp
         | Tok IntOp
         | Sym String
         deriving Show

data IntOp = Int Int
           | Plus Int IntOp
           | Minus Int IntOp
           deriving Show

}
