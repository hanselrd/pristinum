{
module Pristinum.Parser (parseString) where

import Control.Monad.Except
import Data.Text hiding (foldr)
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
    "func"    { TokenReserved "func" }
    "return"  { TokenReserved "return" }
    "struct"  { TokenReserved "struct" }
    "sizeof"  { TokenReserved "sizeof" }
    "void"    { TokenReserved "void" }
    "bool"    { TokenReserved "bool" }
    "char"    { TokenReserved "char" }
    "i8"      { TokenReserved "i8" }
    "i16"     { TokenReserved "i16" }
    "i32"     { TokenReserved "i32" }
    "i64"     { TokenReserved "i64" }
    "u8"      { TokenReserved "u8" }
    "u16"     { TokenReserved "u16" }
    "u32"     { TokenReserved "u32" }
    "u64"     { TokenReserved "u64" }
    "f32"     { TokenReserved "f32" }
    "f64"     { TokenReserved "f64" }
    "nil"     { TokenReserved "nil" }
    "true"    { TokenReserved "true" }
    "false"   { TokenReserved "false" }
    "@"       { TokenSymbol "@" }
    "#"       { TokenSymbol "#" }
    "##"      { TokenSymbol "##" }
    ":"       { TokenSymbol ":" }
    "."       { TokenSymbol "." }
    ","       { TokenSymbol "," }
    "("       { TokenSymbol "(" }
    ")"       { TokenSymbol ")" }
    ":="      { TokenSymbol ":=" }
    "="       { TokenSymbol "=" }
    "+"       { TokenSymbol "+" }
    "-"       { TokenSymbol "-" }
    "*"       { TokenSymbol "*" }
    "/"       { TokenSymbol "/" }
    "%"       { TokenSymbol "%" }
    "**"      { TokenSymbol "**" }
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
    INT       { TokenInt $$ }
    FLOAT     { TokenFloat $$ }
    STRING    { TokenString $$ }
    CHAR      { TokenChar $$ }
    IDENT     { TokenIdentifier $$ }

%left "+" "-" "*" "/" "%" "**" "<<" ">>" "&" "|" "^" "==" "!=" "<" ">" "<=" ">=" "&&" "||"
%left UMINUS "~" "!" DEREF REF
%%

Program : Stmts          { Program $1 }

Stmts :                  { [] }
      | Stmts Stmt       { $1 ++ [$2] }

Stmt : Expr "."                                                  { ExprStmt $1 }
     | Bind ":=" Expr "."                                        { BindStmt $1 $3 }
     | IDENT "=" Expr "."                                        { AssignStmt (pack $1) $3 }
     | "if" Expr ":" Stmts ElifStmts "."                         { IfStmt (($2, $4):$5) Nothing }
     | "if" Expr ":" Stmts ElifStmts "else" Stmts "."            { IfStmt (($2, $4):$5) (Just $7) }
     | "while" Expr ":" Stmts "."                                { WhileStmt $2 $4 }
     | "return" "."                                              { ReturnStmt Nothing }
     | "return" Expr "."                                         { ReturnStmt (Just $2) }
     | "func" IDENT "(" BindCommaList ")" "@" Type ":" Stmts "." { FunctionStmt $7 (pack $2) $4 $9 }
     | "struct" IDENT ":" BindPeriodList "."                     { StructStmt (pack $2) $4 }

ElifStmts :                                     { [] }
          | ElifStmts ElifStmt                  { $1 ++ [$2] }

ElifStmt : "elif" Expr ":" Stmts                { ($2, $4) }

BindCommaList :                                 { [] }
              | BindCommaLoop                   { $1 }

BindCommaLoop : Bind                            { [$1] }
              | BindCommaLoop "," Bind          { $1 ++ [$3] }

BindPeriodList : Bind "."                       { [$1] }
               | BindPeriodList Bind "."        { $1 ++ [$2] }

Expr : "nil"                            { ExprNil }
     | "true"                           { ExprBool True }
     | "false"                          { ExprBool False }
     | CHAR                             { ExprChar $1 }
     | INT                              { ExprInt $1 }
     | FLOAT                            { ExprFloat $1 }
     | STRING                           { ExprString (pack $1) }
     | IDENT                            { ExprVariable (pack $1) }
     | IDENT "(" ExprCommaList ")"      { ExprCall (pack $1) $3 }
     | "<" Type ">" Expr                { ExprCast $2 $4 }
     | Expr "#" Expr                    { ExprAccess $1 $3 }
     | Expr "##" Expr                   { ExprAccess (ExprDeref $1) $3 }
     | "*" Expr %prec DEREF             { ExprDeref $2 }
     | "**" Expr %prec DEREF            { ExprDeref (ExprDeref $2) }
     | "&" Expr %prec REF               { ExprRef $2 }
     | "&&" Expr %prec REF              { ExprRef (ExprRef $2) }
     | "sizeof" "(" Type ")"            { ExprSizeof $3 }
     | Expr "-" %prec UMINUS            { ExprUnaryOp UnOpNegative $1 }
     | Expr "~"                         { ExprUnaryOp UnOpBitwiseNot $1 }
     | Expr "!"                         { ExprUnaryOp UnOpLogicalNot $1 }
     | Expr Expr "+"                    { ExprBinaryOp BinOpAdd $1 $2 }
     | Expr Expr "-"                    { ExprBinaryOp BinOpSubtract $1 $2 }
     | Expr Expr "*"                    { ExprBinaryOp BinOpMultiply $1 $2 }
     | Expr Expr "/"                    { ExprBinaryOp BinOpDivide $1 $2 }
     | Expr Expr "%"                    { ExprBinaryOp BinOpMod $1 $2 }
     | Expr Expr "**"                   { ExprBinaryOp BinOpPower $1 $2 }
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
     | "(" Expr ")"                     { $2 }

ExprCommaList :                                     { [] }
              | ExprCommaLoop                       { $1 }

ExprCommaLoop : Expr                                { [$1] }
              | ExprCommaLoop "," Expr              { $1 ++ [$3] }

Bind : IDENT "@" Type                               { Bind $3 (pack $1) }

Type : "void"                                       { TypeVoid }
     | "bool"                                       { TypeBool }
     | "char"                                       { TypeChar }
     | "i8"                                         { TypeInt8 }
     | "i16"                                        { TypeInt16 }
     | "i32"                                        { TypeInt32 }
     | "i64"                                        { TypeInt64 }
     | "u8"                                         { TypeUint8 }
     | "u16"                                        { TypeUint16 }
     | "u32"                                        { TypeUint32 }
     | "u64"                                        { TypeUint64 }
     | "f32"                                        { TypeFloat32 }
     | "f64"                                        { TypeFloat64 }
     | Type "*"                                     { TypePointer $1 }
     | Type "**"                                    { TypePointer (TypePointer $1) }
     | "struct" IDENT                               { TypeStruct (pack $2) }

{
parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseString = runExcept . parse . lexString
}