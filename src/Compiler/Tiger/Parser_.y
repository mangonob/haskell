{
module Compiler.Tiger.Parser_ where

import Compiler.Tiger.Token
import qualified Compiler.Tiger.AbSyn as A
}

%name parser
%tokentype { Token }
%error { parseError }

%token
while           { While _ }
for             { For _ }
to              { To _ }
break           { Break _ }
let             { Let _ }
in              { In _ }
end             { End _ }
function        { Function _ }
var             { Var _ }
type            { Type _  }
array           { Array _ }
if              { If _ }
then            { Then _ }
else            { Else _ }
do              { Do _ }
of              { Of _ }
nil             { Nil _ }
','             { Comma _ }
':'             { Colon _ }
';'             { Semicolon _  }
'('             { LeftParen _ }
')'             { RightParen _ }
'['             { LeftBracket _ }
']'             { RightBracket _ }
'{'             { LeftBrace _ }
'}'             { RightBrace _ }
'.'             { Dot _ }
'+'             { Plus _ }
'-'             { Minus _ }
'*'             { Times _ }
'/'             { Divide _ }
'='             { Eq _ }
'<>'            { NotEq _ }
'<'             { Lt _ }
'<='            { Le _ }
'>'             { Gt _ }
'>='            { Ge _ }
'&'             { And _ }
'|'             { Or _ }
':='            { Assign _ }
string          { String _ _ }
int             { Int _ _ }
id              { ID _ _ }


%nonassoc do of
%nonassoc else
%nonassoc ':='
%left '&' '|'
%nonassoc '>' '>=' '<' '<=' '<>' '='
%left '+' '-'
%left '*' '/'
%left UMINUS


%%

program:        expr                              { $1 }

expr :: { A.Expr }
expr:           int                               { A.IntExpr (i_value $1) (pos $1) }
|               string                            { A.StringExpr (s_value $1) (pos $1) }
|               nil                               { A.NilExpr (pos $1) }
|               lvalue                            { A.VarExpr $1 }
|               lvalue ':=' expr                  { A.AssignExpr $1 $3 (pos $2) }
|               '(' ')'                           { A.SeqExpr [] (pos $2)}
|               '(' expr_seq ')'                  { A.SeqExpr $2 (pos $3)}
|               '-' expr %prec UMINUS             { A.UMinus $2 (pos $1)}
|               id '(' ')'                        { A.Call (id_value $1) [] (pos $1) }
|               id '(' params ')'                 { A.Call (id_value $1) $3 (pos $1) }
|               expr '+' expr                     { A.OpExpr $1 A.PlusOp $3 (pos $2) }
|               expr '-' expr                     { A.OpExpr $1 A.MinusOp $3 (pos $2) }
|               expr '*' expr                     { A.OpExpr $1 A.TimesOp $3 (pos $2) }
|               expr '/' expr                     { A.OpExpr $1 A.DivideOp $3 (pos $2) }
|               expr '=' expr                     { A.OpExpr $1 A.EqOp $3 (pos $2) }
|               expr '<>' expr                    { A.OpExpr $1 A.NeqOp $3 (pos $2) }
|               expr '<' expr                     { A.OpExpr $1 A.LtOp $3 (pos $2) }
|               expr '<=' expr                    { A.OpExpr $1 A.LeOp $3 (pos $2) }
|               expr '>' expr                     { A.OpExpr $1 A.GtOp $3 (pos $2) }
|               expr '>=' expr                    { A.OpExpr $1 A.GeOp $3 (pos $2) }
|               expr '&' expr                     { A.IFExpr $1 $3 (Just A.zero) (pos $2) }
|               expr '|' expr                     { A.IFExpr $1 A.one (Just $3) (pos $2) }
|               id '{' '}'                        { A.RecordsExpr (id_value $1) [] (pos $1) }
|               id '{' records '}'                { A.RecordsExpr (id_value $1) $3 (pos $1) }
|               id '[' expr ']' of expr           { A.ArrayExpr (id_value $1) $3 $6 (pos $1) }
|               if expr then expr %prec do        { A.IFExpr $2 $4 Nothing (pos $1) }
|               if expr then expr else expr       { A.IFExpr $2 $4 (Just $6) (pos $1) }
|               while expr do expr                { A.WhileExpr $2 $4 (pos $1) }
|               for id ':=' expr to expr do expr  { A.ForExpr (id_value $2) $4 $6 $8 False (pos $1)}
|               break                             { A.BreakExpr (pos $1) }
|               let decs in end                   { A.LetExpr $2 (A.SeqExpr [] (pos $4)) (pos $1) }
|               let decs in expr_seq end          { A.LetExpr $2 (A.SeqExpr $4 (pos $5)) (pos $1) }

lvalue :: { A.Var }
lvalue:         id                                { simpleVar $1 }
|               lvalue_                           { $1 }

lvalue_ :: { A.Var }
lvalue_:        id '.' id                         { A.FieldVar (simpleVar $1) (id_value $3) (pos $2) }
|               id '[' expr ']'                   { A.IndexedVar (simpleVar $1) $3 (pos $2)} 
|               lvalue_ '.' id                    { A.FieldVar $1 (id_value $3) (pos $2) }
|               lvalue_ '[' expr ']'              { A.IndexedVar $1 $3 (pos $2) }


expr_seq :: { [A.Expr] }
expr_seq:       expr                              { [$1] }
|               expr_seq ';' expr                 { $1 ++ [$3]}

params :: { [A.Expr] }
params:         expr                              { [$1] }
|               params ',' expr                   { $1 ++ [$3] }

records :: { [A.Field] }
records:        field                             { [$1] }
|               records ',' field                 { $1 ++ [$3] }

field :: { A.Field }
field:          id '=' expr                      { A.Field (id_value $1) $3 (pos $1) }

decs :: { [A.Dec] }
decs:           decs dec          { $1 ++ [$2] }
|               {- empty -}       { [] }

dec :: { A.Dec }
dec:            type id '=' ty                                { A.TypeDec (id_value $2) $4 (pos $1)}
|               var id ':=' expr                              { A.VarDec (id_value $2) $4 Nothing False (pos $1)}
|               var id ':' type_id ':=' expr                  { A.VarDec (id_value $2) $6 (Just (id_value $4)) False (pos $1)}
|               function id '(' ty_fields ')' '=' expr       { A.FuncDec (id_value $2) $4 Nothing $7 (pos $1)} 
|               function id '(' ty_fields ')' 
                  ':' type_id '=' expr                       { A.FuncDec (id_value $2) $4 (Just (id_value $7)) $9 (pos $1)}

ty :: { A.Type }
ty:             type_id                       { A.SimpleType (id_value $1) (pos $1) }
|               '{' ty_fields '}'             { A.Records $2 (pos $1) }
|               array of type_id              { A.Array (id_value $3) (pos $1) }

ty_fields :: { [A.Record] }
ty_fields:      ty_fields_                    { $1 }
|               {- empty -}                   { [] }

ty_fields_ :: { [A.Record] }
ty_fields_:     ty_field                      { [$1] }
|               ty_fields_ ',' ty_field       { $1 ++ [$3] }

ty_field :: { A.Record }
ty_field:       id ':' type_id                { A.Record (id_value $1) (id_value $3) False (pos $1) }

type_id :: { Token }
type_id:        id                            { $1 }

{
parseError :: [Token] -> a
parseError ts = error $ "syntax error on " ++ show (take 3 ts)

simpleVar :: Token -> A.Var
simpleVar t = A.SimpleVar (id_value t) (pos t)
}