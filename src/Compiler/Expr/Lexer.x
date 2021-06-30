{
module Compiler.Expr.Lexer where

import Compiler.Expr.Token
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]
@id = ($alpha | \_) ($alpha | $digit | \_) *

token :-

$white+       ;

let           { return Let }
in            { return In }
@id           { Var }
$digit+       { \s -> Int (read s :: Int) }
"="           { return Eq }
"+"           { return Plus }
"-"           { return Minus }
"*"           { return Times }
"/"           { return Div }
"("           { return OB }
")"           { return CB }

{
lexer = alexScanTokens
}