{
module  Compiler.Expr.Parser where

import Compiler.Expr.Token
import qualified Compiler.Expr.AbSyn as A
}

%name parser
%tokentype { Token }
%error { parseError }

%token
int             { Int $$ }
'+'             { Plus }
'-'             { Minus }
'*'             { Times }
'/'             { Div }
'('             { OB }
')'             { CB }

%left '+' '-'
%left '*' '/'

%%

program:        expr                  { $1 }

expr:           '(' expr ')'          { $2 }
|               expr '+' expr         { A.OpExpr $1 A.Plus $3 }
|               expr '-' expr         { A.OpExpr $1 A.Minus $3 }
|               expr '*' expr         { A.OpExpr $1 A.Times $3 }
|               expr '/' expr         { A.OpExpr $1 A.Div $3 }
|               int                   { A.IntExpr $1 }

{
parseError :: [Token] -> a
parseError = undefined
}