{
module  Compiler.Expr.Parser where

import Compiler.Expr.Token
import Compiler.Expr.Lexer (lexer)
import Control.Monad.State (MonadState (get, put), State, evalState)
}

%name parser
%monad { Environment }
%tokentype { Token }
%error { parseError }

%token
let             { Let }
in              { In }
int             { Int $$ }
var             { Var $$ }
'='             { Eq }
'+'             { Plus }
'-'             { Minus }
'*'             { Times }
'/'             { Div }
'('             { OB }
')'             { CB }

%left '+' '-'
%left '*' '/'
%left in
%left UMINUS

%%

program:        expr                            { $1 }

expr :: { Int }
expr:           '(' expr ')'                    { $2 }
|               let in expr                     { $3 }
|               let decs in expr                { seq $2 $4 }
|               expr '+' expr                   { $1 +  $3 }
|               expr '-' expr                   { $1 - $3 }
|               expr '*' expr                   { $1 *  $3 }
|               expr '/' expr                   { $1 `div` $3 }
|               int                             { $1 }
|               '-' expr %prec UMINUS           { (- $2) }
|               var                             {% do
                                                   env <- get
                                                   case lookup $1 env of
                                                     Just v -> return v
                                                     Nothing -> error $ "undefined var " ++ $1 }

decs :: { () }
decs:           dec                             { $1 }
|               decs dec                        { seq $2 $1 }

dec :: { () }
dec:            var '=' expr                    {% do
                                                     env <- get 
                                                     put (($1, $3) : env) }

{
parseError :: [Token] -> Environment a
parseError = undefined

type Environment = State [(String, Int)]
}