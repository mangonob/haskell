{
module  Compiler.Expr.Parser where

import Compiler.Expr.Token
import Compiler.Expr.Lexer (lexer)
import Control.Monad.State (MonadState (get, put), State, evalState)
}

%name calc
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

program:        expr                            { evalState $1 [] }

expr :: { ExprState }
expr:           '(' expr ')'                    { $2 }
|               let in expr                     { $3 }
|               let decs in expr                { $2 >> $4 }
|               expr '+' expr                   { apply (+) $1 $3 }
|               expr '-' expr                   { apply (-) $1 $3 }
|               expr '*' expr                   { apply (*) $1 $3 }
|               expr '/' expr                   { apply div $1 $3 }
|               int                             { return $1 }
|               '-' expr %prec UMINUS           { do
                                                    v <- $2
                                                    return (-v) }
|               var                             { readV $1 }

decs:           dec                             { $1 }
|               decs dec                        { $2 >> $1 }

dec:            var '=' expr                    { addV $1 $3 }

{
parseError :: [Token] -> a
parseError = undefined

type ExprState = State [(String, Int)] Int

apply :: (Int -> Int -> Int) -> ExprState -> ExprState -> ExprState
apply f s1 s2 = do
  e1 <- s1
  e2 <- s2
  return (f e1 e2)

addV :: String -> ExprState -> State [(String, Int)] ()
addV v expr = do
  exp <- expr
  env <- get
  put ((v, exp) : env)

readV :: String -> ExprState
readV v = do
  env <- get
  case lookup v env of
    Just value -> return value
    Nothing -> error $ "undefined var " ++ v
}