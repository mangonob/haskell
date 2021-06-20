module Compiler.Tiger.Parser where

import qualified Compiler.Tiger.AbSyn as A
import Compiler.Tiger.Lexer (Pos (Pos), Token)
import Control.Monad.State (State)

{-
Prog -> ''
Prog -> Expr
Prog -> Prog Expr

Expr -> Logic
Expr -> Expr & Logic
Expr -> Expr | Logic

Logic -> Comp
Logic -> Logic = Comp
Logic -> Logic > Comp
Logic -> Logic >= Comp
Logic -> Logic < Comp
Logic -> Logic <= Comp
Logic -> Logic <> Comp

Comp -> Term
Comp -> Comp + Term
Comp -> Comp - Term

Term -> Factor
Term -> Term * Factor
Term -> Term / Factor

Factor -> Var
Factor -> Var := Expr
Factor -> nil
Factor -> int
Factor -> string
Factor -> - Factor
Factor -> Let
Factor -> If
Factor -> While
Factor -> For
Factor -> type_id { Fields }
Factor -> ( SeqExpr )

Fields -> ''
Fields -> Field
Fields -> Fields , Field

Field -> type_id = Expr

SeqExpr -> ''
SeqExpr -> Expr
SeqExpr -> SeqExpr ; Expr

Decs -> ''
Desc -> Dec
Desc -> Decs Dec

Dec -> TypeDec
Dec -> VarDec
Dec -> FuncDec

TypeDec -> type type_id = Type

Type -> type_id
Type -> { Records }
Type -> array of type_id

Records -> ''
Records -> Record
Records -> Records, Record

Record -> id : type_id

VarDec -> var id := Expr
VarDec -> var id : type_id := Expr

FuncDec -> function id ( Records ) = Expr
FuncDec -> function id ( Records ) : type_id = Expr

Var -> id
Var -> Var . id
Var -> Var [ exp ]

If -> if Expr then Expr
If -> if Expr then Expr else Expr

While -> while Expr do Expr

For -> for id := Expr to Expr do Expr

Break -> break

Let -> let Decs in SeqExp end
-}

expr :: State ([Token], Pos) A.Expr
expr = undefined