{
module Pristinum.Lexer (Token(..), lexString) where

--import Data.Char (digitToInt)
import Data.String.Utils
import Numeric
}

%wrapper "basic"

$alpha = [a-zA-Z]
$binary = 0-1
$octal = 0-7
$decimal = 0-9
$hexadecimal = [0-9a-fA-F]
$newline = [\r\n]

tokens :-
    $white+                                                  ;--{ \s -> TokenWhiteSpace s }
    "/*" ( $newline | [^\*] | \*+ ($newline | [^\/]) )* "*/" ;--{ \s -> TokenComment (tail (tail s)) }
    "//" [^$newline]* $newline                               ;--{ \s -> TokenComment (tail (tail s)) }
    if                                                       { \s -> TokenReserved s }
    elif                                                     { \s -> TokenReserved s }
    else                                                     { \s -> TokenReserved s }
    while                                                    { \s -> TokenReserved s }
    func                                                     { \s -> TokenReserved s }
    return                                                   { \s -> TokenReserved s }
    struct                                                   { \s -> TokenReserved s }
    sizeof                                                   { \s -> TokenReserved s }
    void                                                     { \s -> TokenReserved s }
    bool                                                     { \s -> TokenReserved s }
    char                                                     { \s -> TokenReserved s }
    i8                                                       { \s -> TokenReserved s }
    i16                                                      { \s -> TokenReserved s }
    i32                                                      { \s -> TokenReserved s }
    i64                                                      { \s -> TokenReserved s }
    u8                                                       { \s -> TokenReserved s }
    u16                                                      { \s -> TokenReserved s }
    u32                                                      { \s -> TokenReserved s }
    u64                                                      { \s -> TokenReserved s }
    f32                                                      { \s -> TokenReserved s }
    f64                                                      { \s -> TokenReserved s }
    nil                                                      { \s -> TokenReserved s }
    true                                                     { \s -> TokenReserved s }
    false                                                    { \s -> TokenReserved s }
    "@"                                                      { \s -> TokenSymbol s }
    "#"                                                      { \s -> TokenSymbol s }
    "##"                                                     { \s -> TokenSymbol s }
    ":"                                                      { \s -> TokenSymbol s }
    "."                                                      { \s -> TokenSymbol s }
    ","                                                      { \s -> TokenSymbol s }
    "("                                                      { \s -> TokenSymbol s }
    ")"                                                      { \s -> TokenSymbol s }
    ":="                                                     { \s -> TokenSymbol s }
    "="                                                      { \s -> TokenSymbol s }
    "+"                                                      { \s -> TokenSymbol s }
    "-"                                                      { \s -> TokenSymbol s }
    "*"                                                      { \s -> TokenSymbol s }
    "/"                                                      { \s -> TokenSymbol s }
    "%"                                                      { \s -> TokenSymbol s }
    "**"                                                     { \s -> TokenSymbol s }
    "<<"                                                     { \s -> TokenSymbol s }
    ">>"                                                     { \s -> TokenSymbol s }
    "&"                                                      { \s -> TokenSymbol s }
    "|"                                                      { \s -> TokenSymbol s }
    "^"                                                      { \s -> TokenSymbol s }
    "~"                                                      { \s -> TokenSymbol s }
    "=="                                                     { \s -> TokenSymbol s }
    "!="                                                     { \s -> TokenSymbol s }
    "<"                                                      { \s -> TokenSymbol s }
    ">"                                                      { \s -> TokenSymbol s }
    "<="                                                     { \s -> TokenSymbol s }
    ">="                                                     { \s -> TokenSymbol s }
    "&&"                                                     { \s -> TokenSymbol s }
    "||"                                                     { \s -> TokenSymbol s }
    "!"                                                      { \s -> TokenSymbol s }
    --0 [bB] [$binary] [$binary \_]*                                                                  { \s -> TokenInt (fst (head (readInt 2 (`elem` "01") digitToInt (replace "_" "" (tail (tail s)))))) }
    --0 [oO] [$octal] [$octal \_]*                                                                    { \s -> TokenInt (fst (head (readOct (replace "_" "" (tail (tail s)))))) }
    $decimal [$decimal \_]*                                                                           { \s -> TokenInt (fst (head (readDec (replace "_" "" s)))) }
    --0 [xX] [$hexadecimal] [$hexadecimal \_]*                                                        { \s -> TokenInt (fst (head (readHex (replace "_" "" (tail (tail s)))))) }
    $decimal [$decimal \_]* \. ( $decimal [$decimal \_]* )? ( [eE] [\+\-]? $decimal [$decimal \_]* )? { \s -> TokenFloat (fst (head (readFloat (replace "_" "" s)))) }
    \" ( \\. | [^\"\\] )* \"                                                                          { \s -> TokenString (init (tail s)) }
    \' [^\'\\] \'                                                                                     { \s -> TokenChar (head (init (tail s))) }
    [$alpha \_] [$alpha $decimal \_]*                                                                 { \s -> TokenIdentifier s }

{
data Token
    = TokenWhiteSpace String
    | TokenComment String
    | TokenReserved String
    | TokenSymbol String
    | TokenInt Int
    | TokenFloat Double
    | TokenString String
    | TokenChar Char
    | TokenIdentifier String
    deriving (Show, Eq)

lexString = alexScanTokens
}