{
module Compiler.JSON.Lexer where

import Compiler.JSON.Token
}

%wrapper "posn"

$alpha = [a-zA-Z]
$digit = [0-9]
@digits = $digit+
@frac = \- ? @digits (\. @digits) ? ([eE] @digits) ?
@int = \- ? @digits
@hex = \- ? 0x [0-9a-fA-F]+
@id = ($alpha | \_) ($alpha | $digit | \_)*

tokens :-

$white                    ;

true                      {tokenWith (return True) Bool}
false                     {tokenWith (return False) Bool}
null                      {token Null}

"("                       {token LeftParen}
")"                       {token LeftParen}
"["                       {token LeftBracket}
"]"                       {token RightBracket}
"{"                       {token LeftBrace}
"}"                       {token RightBrace }
":"                       {token Colon}
","                       {token Comma}
@digits                   {tokenWith read Number}
@hex                      {tokenWith read Number}
@frac                     {tokenWith read Number}
@id                       {tokenWith id String}
\" \"                     {tokenWith unquote String}
\" ([^\"]| \\ \")* \"     {tokenWith unquote String}

{
toPos :: AlexPosn -> Pos
toPos (AlexPn p r c) = Pos p r c

lexer = alexScanTokens

token :: (Pos -> Token) -> AlexPosn -> String -> Token
token f (AlexPn p r c) _ = f (Pos p r c)

tokenWith ::  (String -> a) -> (a -> Pos -> Token) -> AlexPosn -> String -> Token
tokenWith vf f (AlexPn p r c) s = f (vf s) (Pos p r c)

unquote :: String -> String
unquote (_:xs) = init xs
}