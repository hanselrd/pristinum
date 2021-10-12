{
module Pristinum.Parser (parseString) where

import Control.Monad.Except
import Pristinum.AST
import Pristinum.Lexer
}

%name parse

%tokentype { Token }

%monad { Except String } { (>>=) } { return }
%error { parseError }

%token
    "if"      { TokenReserved "if" }
    "elif"    { TokenReserved "elif" }
    "else"    { TokenReserved "else" }
    "while"   { TokenReserved "while" }
    "do"      { TokenReserved "do" }
    "macro"   { TokenReserved "macro" }
    "end"     { TokenReserved "end" }
    "null"    { TokenReserved "null" }
    "true"    { TokenReserved "true" }
    "false"   { TokenReserved "false" }
    "drop"    { TokenReserved "drop" }
    "neg"     { TokenReserved "neg" }
    "load"    { TokenReserved "load" }
    "store"   { TokenReserved "store" }
    "@"       { TokenSymbol "@" }
    "#"       { TokenSymbol "#" }
    ":"       { TokenSymbol ":" }
    "("       { TokenSymbol "(" }
    ")"       { TokenSymbol ")" }
    "+"       { TokenSymbol "+" }
    "-"       { TokenSymbol "-" }
    "*"       { TokenSymbol "*" }
    "/"       { TokenSymbol "/" }
    "%"       { TokenSymbol "%" }
    "<<"      { TokenSymbol "<<" }
    ">>"      { TokenSymbol ">>" }
    "&"       { TokenSymbol "&" }
    "|"       { TokenSymbol "|" }
    "^"       { TokenSymbol "^" }
    "~"       { TokenSymbol "~" }
    "=="      { TokenSymbol "==" }
    "!="      { TokenSymbol "!=" }
    "<"       { TokenSymbol "<" }
    ">"       { TokenSymbol ">" }
    "<="      { TokenSymbol "<=" }
    ">="      { TokenSymbol ">=" }
    "&&"      { TokenSymbol "&&" }
    "||"      { TokenSymbol "||" }
    "!"       { TokenSymbol "!" }
    NUM       { TokenNumber $$ }
    STRING    { TokenString $$ }
    IDENT     { TokenIdentifier $$ }

%left "+" "-" "*" "/" "%" "<<" ">>" "&" "|" "^" "==" "!=" "<" ">" "<=" ">=" "&&" "||"
%right UMINUS "~" "!"
%%

Program : Stmts          { Program $1 }

Stmts :                  { [] }
      | Stmts Stmt       { $1 ++ [$2] }

Stmt : "(" Stmt ")"                                        { $2 }
     | "null"                                              { PushStmt (LitNull) }
     | "true"                                              { PushStmt (LitBool True) }
     | "false"                                             { PushStmt (LitBool False) }
     | NUM                                                 { PushStmt (LitNumber $1) }
     | STRING                                              { PushStmt (LitString $1) }
     | IDENT                                               { PushStmt (LitIdentifier $1) }
     | "drop"                                              { DropStmt }
     | "neg" %prec UMINUS                                  { UnaryOpStmt UnOpNegative }
     | "~"                                                 { UnaryOpStmt UnOpBitwiseNot }
     | "!"                                                 { UnaryOpStmt UnOpLogicalNot }
     | "+"                                                 { BinaryOpStmt BinOpAdd }
     | "-"                                                 { BinaryOpStmt BinOpSubtract }
     | "*"                                                 { BinaryOpStmt BinOpMultiply }
     | "/"                                                 { BinaryOpStmt BinOpDivide }
     | "%"                                                 { BinaryOpStmt BinOpMod }
     | "<<"                                                { BinaryOpStmt BinOpBitwiseShiftLeft }
     | ">>"                                                { BinaryOpStmt BinOpBitwiseShiftRight }
     | "&"                                                 { BinaryOpStmt BinOpBitwiseAnd }
     | "|"                                                 { BinaryOpStmt BinOpBitwiseOr }
     | "^"                                                 { BinaryOpStmt BinOpBitwiseXor }
     | "=="                                                { BinaryOpStmt BinOpEqual }
     | "!="                                                { BinaryOpStmt BinOpNotEqual }
     | "<"                                                 { BinaryOpStmt BinOpLess }
     | ">"                                                 { BinaryOpStmt BinOpGreater }
     | "<="                                                { BinaryOpStmt BinOpLessEqual }
     | ">="                                                { BinaryOpStmt BinOpGreaterEqual }
     | "&&"                                                { BinaryOpStmt BinOpLogicalAnd }
     | "||"                                                { BinaryOpStmt BinOpLogicalOr }
     | "if" Stmts "do" Stmts ElifStmts "end"               { IfStmt $2 $4 (foldr (\elem acc -> [elem { ifStmtElseBody = acc }]) [] $5) }
     | "if" Stmts "do" Stmts ElifStmts "else" Stmts "end"  { IfStmt $2 $4 (foldr (\elem acc -> [elem { ifStmtElseBody = acc }]) $7 $5) }
     | "while" Stmts "do" Stmts "end"                      { WhileStmt $2 $4 }
     | "macro" IDENT "do" Stmts "end"                      { MacroStmt $2 $4 }

ElifStmts :                     { [] }
          | ElifStmts ElifStmt  { $1 ++ [$2] }

ElifStmt : "elif" Stmts "do" Stmts  { IfStmt $2 $4 [] }

{
parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseString = runExcept . parse . lexString
}