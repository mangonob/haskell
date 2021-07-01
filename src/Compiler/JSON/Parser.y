{
module Compiler.JSON.Parser where

import Compiler.JSON.Token
import qualified Compiler.JSON.AbSyn as A
import Compiler.JSON.Lexer (lexer)
}

%name parser
%tokentype { Token }
%error { parseError }

%token
bool                      { Bool _ _ }
null                      { Null _ }
string                    { String _ _ }
number                    { Number _ _ }
'['                       { LeftBracket _ }
']'                       { RightBracket _ }
'{'                       { LeftBrace _ }
'}'                       { RightBrace _ }
':'                       { Colon _ }
','                       { Comma _ }

%%
program:                  json                          { $1 }

json:                     null                          { A.Null (pos $1)}
|                         bool                          { A.Bool (bool_value $1) (pos $1)}
|                         string                        { A.String (string_value $1) (pos $1) }
|                         number                        { A.Number (number_value $1) (pos $1) }
|                         array                         { $1 }
|                         object                        { $1 }

array:                    '[' ']'                       { A.Array [] (pos $1) }
array:                    '[' elements ']'              { A.Array $2 (pos $1) }

elements:                 json                          { [$1] }
elements:                 elements ',' json             { $1 ++ [$3] }

object:                   '{' '}'                       { A.Object [] (pos $1) }
object:                   '{' key_values '}'            { A.Object $2 (pos $1) }

key_values:               key_value                     { [$1] }
key_values:               key_values ',' key_value      { $1 ++ [$3] }

key_value:                string ':' json               { (string_value $1, $3) }


{
parseError :: [Token] -> a
parseError = undefined
}