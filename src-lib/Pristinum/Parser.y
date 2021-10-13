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
    "let"     { TokenReserved "let" }
    "if"      { TokenReserved "if" }
    "elif"    { TokenReserved "elif" }
    "else"    { TokenReserved "else" }
    "while"   { TokenReserved "while" }
    "do"      { TokenReserved "do" }
    "func"    { TokenReserved "func" }
    "end"     { TokenReserved "end" }
    "return"  { TokenReserved "return" }
    "null"    { TokenReserved "null" }
    "true"    { TokenReserved "true" }
    "false"   { TokenReserved "false" }
    "."       { TokenSymbol "." }
    ","       { TokenSymbol "," }
    "("       { TokenSymbol "(" }
    ")"       { TokenSymbol ")" }
    "="       { TokenSymbol "=" }
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

Stmt : Expr "."                                                  { ExprStmt $1 }
     | "let" IDENT "=" Expr "."                                  { LetStmt $2 $4 }
     | IDENT "=" Expr "."                                        { AssignStmt $1 $3 }
     | "if" Expr "do" Stmts ElifStmts "end"                      { IfStmt $2 $4 (foldr (\elem acc -> Just [elem { ifStmtElseBody = acc }]) Nothing $5) }
     | "if" Expr "do" Stmts ElifStmts "else" Stmts "end"         { IfStmt $2 $4 (foldr (\elem acc -> Just [elem { ifStmtElseBody = acc }]) (Just $7) $5) }
     | "while" Expr "do" Stmts "end"                             { WhileStmt $2 $4 }
     | "func" IDENT "(" ParameterCommaList ")" "do" Stmts "end"  { FunctionStmt $2 $4 $7 }
     | "return" "."                                              { ReturnStmt Nothing }
     | "return" Expr "."                                         { ReturnStmt (Just $2) }

ElifStmts :                                     { [] }
          | ElifStmts ElifStmt                  { $1 ++ [$2] }

ElifStmt : "elif" Expr "do" Stmts               { IfStmt $2 $4 Nothing }

ParameterCommaList :                               { [] }
                   | ParameterCommaLoop            { $1 }

ParameterCommaLoop : IDENT                         { [$1] }
                   | ParameterCommaLoop "," IDENT  { $1 ++ [$3] }

Expr : "(" Expr ")"                     { $2 }
     | "null"                           { ExprNull }
     | "true"                           { ExprBool True }
     | "false"                          { ExprBool False }
     | NUM                              { ExprNumber $1 }
     | STRING                           { ExprString $1 }
     | IDENT                            { ExprVariable $1 }
     | IDENT "(" ExprCommaList ")"      { ExprCall $1 $3 }
     --| Expr "-" %prec UMINUS            { ExprUnaryOp UnOpNegative $1 }
     | Expr "~"                         { ExprUnaryOp UnOpBitwiseNot $1 }
     | Expr "!"                         { ExprUnaryOp UnOpLogicalNot $1 }
     | Expr Expr "+"                    { ExprBinaryOp BinOpAdd $1 $2 }
     | Expr Expr "-"                    { ExprBinaryOp BinOpSubtract $1 $2 }
     | Expr Expr "*"                    { ExprBinaryOp BinOpMultiply $1 $2 }
     | Expr Expr "/"                    { ExprBinaryOp BinOpDivide $1 $2 }
     | Expr Expr "%"                    { ExprBinaryOp BinOpMod $1 $2 }
     | Expr Expr "<<"                   { ExprBinaryOp BinOpBitwiseShiftLeft $1 $2 }
     | Expr Expr ">>"                   { ExprBinaryOp BinOpBitwiseShiftRight $1 $2 }
     | Expr Expr "&"                    { ExprBinaryOp BinOpBitwiseAnd $1 $2 }
     | Expr Expr "|"                    { ExprBinaryOp BinOpBitwiseOr $1 $2 }
     | Expr Expr "^"                    { ExprBinaryOp BinOpBitwiseXor $1 $2 }
     | Expr Expr "=="                   { ExprBinaryOp BinOpEqual $1 $2 }
     | Expr Expr "!="                   { ExprBinaryOp BinOpNotEqual $1 $2 }
     | Expr Expr "<"                    { ExprBinaryOp BinOpLess $1 $2 }
     | Expr Expr ">"                    { ExprBinaryOp BinOpGreater $1 $2 }
     | Expr Expr "<="                   { ExprBinaryOp BinOpLessEqual $1 $2 }
     | Expr Expr ">="                   { ExprBinaryOp BinOpGreaterEqual $1 $2 }
     | Expr Expr "&&"                   { ExprBinaryOp BinOpLogicalAnd $1 $2 }
     | Expr Expr "||"                   { ExprBinaryOp BinOpLogicalOr $1 $2 }

ExprCommaList :                                     { [] }
              | ExprCommaLoop                       { $1 }

ExprCommaLoop : Expr                                { [$1] }
              | ExprCommaLoop "," Expr              { $1 ++ [$3] }

{
parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseString = runExcept . parse . lexString
}