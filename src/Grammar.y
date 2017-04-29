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
    else        { TokenElse }
    endif       { TokenEndIf }
    while       { TokenWhile }
    endwhile    { TokenEndWhile }
    print       { TokenPrint }
    if          { TokenIf }
    '='         { TokenAssign }
    '+'         { TokenPlus }
    '-'         { TokenMinus }
    '*'         { TokenMultiply }
    '/'         { TokenDivide }
    '<'         { TokenLess }
    '>'         { TokenGreater }
    '<='        { TokenLessEqual }
    '>='        { TokenGreaterEqual}


%left '+' '-'
%left '*' '/'
%left '<' '>' '<=' '>='
%left '&&' '||'

%%

prog : exp prog              { $1 : $2 } 
     | exp                   { [$1] }

exp : var sym '=' int_op nl     { Assign $2 $4 }
    | int_op nl                 { Tok $1 }
    | if_expression             { IfExp $1 } 

if_expression : if if_cond if_body endif nl                     { If $2 $3 }
              | if if_cond if_body else nl else_body endif nl   { IfElse $2 $3 $6 }

if_cond : cond nl { $1 }

if_body : bloq if_body { $1 : $2 }
       |  bloq        { [$1] }

else_body : bloq else_body { $1: $2 }
          | bloq          { [$1] }

bloq : var sym '=' int_op nl     { BloqAssign $2 $4 }
     | int_op nl                 { BloqTok $1 }

int_op : sym                  { Sym $1 }
       | int                     { Int $1 }
       | int_op '+' int_op          { Plus $1 $3 } 
       | int_op '-' int_op          { Minus $1 $3 }
       | int_op '*' int_op          { Multiply $1 $3 }
       | int_op '/' int_op          { Divide $1 $3 }

cond : int_op '<'  int_op             { Less $1 $3 }
     | int_op '<=' int_op             { LessEqual $1 $3 }
     | int_op '>'  int_op             { Greater $1 $3 }
     | int_op '>=' int_op             { GreaterEqual $1 $3 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp = Assign String IntOp
         | Tok IntOp
         | IfExp IfBody
         deriving (Eq, Show)

data IfBody = If Conditional [Bloq]
            | IfElse Conditional [Bloq] [Bloq]
            deriving (Eq, Show)

data Bloq = BloqAssign String IntOp
          | BloqTok IntOp
          deriving (Eq, Show)

data Conditional = Less IntOp IntOp
          | LessEqual IntOp IntOp
          | Greater IntOp IntOp
          | GreaterEqual IntOp IntOp
          deriving (Eq, Show)

data IntOp = Int Int
           | Sym String
           | Plus IntOp IntOp
           | Minus IntOp IntOp
           | Multiply IntOp IntOp
           | Divide IntOp IntOp
           deriving (Eq, Show)

}
