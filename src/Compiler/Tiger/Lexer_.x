{
module Compiler.Tiger.Lexer_ where

import Compiler.Tiger.Token
}

%wrapper "posn"

$digit  = [0-9]
$alpha  = [a-zA-Z]
@id     = ($alpha | \_) ($alpha | $digit | \_)*
$any    = [ . \n ]

token :-

$white+                         ;
"//" .*                         ;

"/*" ([$any # \*] | \* [$any # \/]) * "*/"                            
                                ;

while                           { token While }
for                             { token For }
to                              { token To }
break                           { token Break }
let                             { token Let }
in                              { token In }
end                             { token End }
function                        { token Function }
var                             { token Var }
type                            { token Type }
array                           { token Array }
if                              { token If }
then                            { token Then }
else                            { token Else }
do                              { token Do }
of                              { token Of }
nil                             { token Nil }

","                             { token Comma }
":"                             { token Colon }
";"                             { token Semicolon }
"("                             { token LeftParen }
")"                             { token RightParen }
"["                             { token LeftBracket }
"]"                             { token RightBracket }
"{"                             { token LeftBrace }
"}"                             { token RightBrace }
"."                             { token Dot }
"+"                             { token Plus }
"-"                             { token Minus }
"*"                             { token Times }
"/"                             { token Divide }
"="                             { token Eq }
">"                             { token Gt }
">="                            { token Ge }
"<"                             { token Lt }
"<="                            { token Le }
"<>"                            { token NotEq }
"&"                             { token And }
"|"                             { token Or }
":="                            { token Assign }
\" \"                           { tokenWith unquote String }
\" ( [^\"] | \\ \") * \"        { tokenWith unquote String}
$digit+                         { tokenWith read Int }
@id                             { tokenWith id ID }

{
lexer :: String -> [Token]
lexer  = alexScanTokens

token :: (Pos -> Token) -> AlexPosn -> String -> Token
token f (AlexPn p r c) _ = f (Pos p r c)

tokenWith :: (String -> a) -> (a -> Pos -> Token) -> AlexPosn -> String -> Token
tokenWith vf f (AlexPn p r c) s = f (vf s) (Pos p r c)

unquote :: String -> String
unquote (_:xs) = init xs
}