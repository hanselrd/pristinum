{
module Pristinum.Lexer (Token(..), lexString) where

import Data.Char
import Data.String.Utils
import Numeric
}

%wrapper "basic"

$binary = 0-1
$octal = 0-7
$decimal = 0-9
$hexadecimal = [0-9a-fA-F]
$alpha = [a-zA-Z]

tokens :-
    $white+                                      ;--{ \s -> TokenWhiteSpace s }
    "//".*                                       ;--{ \s -> TokenComment (tail (tail s)) }
    let                                          { \s -> TokenReserved s }
    if                                           { \s -> TokenReserved s }
    elif                                         { \s -> TokenReserved s }
    else                                         { \s -> TokenReserved s }
    while                                        { \s -> TokenReserved s }
    do                                           { \s -> TokenReserved s }
    func                                         { \s -> TokenReserved s }
    end                                          { \s -> TokenReserved s }
    return                                       { \s -> TokenReserved s }
    null                                         { \s -> TokenReserved s }
    true                                         { \s -> TokenReserved s }
    false                                        { \s -> TokenReserved s }
    "."                                          { \s -> TokenSymbol s }
    ","                                          { \s -> TokenSymbol s }
    "("                                          { \s -> TokenSymbol s }
    ")"                                          { \s -> TokenSymbol s }
    "="                                          { \s -> TokenSymbol s }
    "+"                                          { \s -> TokenSymbol s }
    "-"                                          { \s -> TokenSymbol s }
    "*"                                          { \s -> TokenSymbol s }
    "/"                                          { \s -> TokenSymbol s }
    "%"                                          { \s -> TokenSymbol s }
    "<<"                                         { \s -> TokenSymbol s }
    ">>"                                         { \s -> TokenSymbol s }
    "&"                                          { \s -> TokenSymbol s }
    "|"                                          { \s -> TokenSymbol s }
    "^"                                          { \s -> TokenSymbol s }
    "~"                                          { \s -> TokenSymbol s }
    "=="                                         { \s -> TokenSymbol s }
    "!="                                         { \s -> TokenSymbol s }
    "<"                                          { \s -> TokenSymbol s }
    ">"                                          { \s -> TokenSymbol s }
    "<="                                         { \s -> TokenSymbol s }
    ">="                                         { \s -> TokenSymbol s }
    "&&"                                         { \s -> TokenSymbol s }
    "||"                                         { \s -> TokenSymbol s }
    "!"                                          { \s -> TokenSymbol s }
    0 [bB] [$binary] [$binary \_]*               { \s -> TokenNumber (fst (head (readInt 2 (`elem` "01") digitToInt (replace "_" "" (tail (tail s)))))) }
    0 [oO] [$octal] [$octal \_]*                 { \s -> TokenNumber (fst (head (readOct (replace "_" "" (tail (tail s)))))) }
    [$decimal] [$decimal \_]*                    { \s -> TokenNumber (fst (head (readDec (replace "_" "" s)))) }
    0 [xX] [$hexadecimal] [$hexadecimal \_]*     { \s -> TokenNumber (fst (head (readHex (replace "_" "" (tail (tail s)))))) }
    \" (\\.|[^\"\\])* \"                         { \s -> TokenString (init (tail s)) }
    [$alpha $decimal \_]+                        { \s -> TokenIdentifier s }

{
data Token
    = TokenWhiteSpace String
    | TokenComment String
    | TokenReserved String
    | TokenSymbol String
    | TokenNumber Integer
    | TokenString String
    | TokenIdentifier String
    deriving (Show, Eq)

lexString = alexScanTokens
}