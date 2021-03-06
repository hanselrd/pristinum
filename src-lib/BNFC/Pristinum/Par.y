-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.3).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module BNFC.Pristinum.Par
  ( happyError
  , myLexer
  , pProgram
  ) where

import Prelude

import qualified BNFC.Pristinum.Abs
import BNFC.Pristinum.Lex
import qualified Data.Text

}

%name pProgram Program
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '!'       { PT _ (TS _ 1)     }
  '!='      { PT _ (TS _ 2)     }
  '%'       { PT _ (TS _ 3)     }
  '&'       { PT _ (TS _ 4)     }
  '&&'      { PT _ (TS _ 5)     }
  '('       { PT _ (TS _ 6)     }
  ')'       { PT _ (TS _ 7)     }
  '*'       { PT _ (TS _ 8)     }
  '**'      { PT _ (TS _ 9)     }
  '+'       { PT _ (TS _ 10)    }
  '++'      { PT _ (TS _ 11)    }
  ','       { PT _ (TS _ 12)    }
  '-'       { PT _ (TS _ 13)    }
  '--'      { PT _ (TS _ 14)    }
  '->'      { PT _ (TS _ 15)    }
  '.'       { PT _ (TS _ 16)    }
  '/'       { PT _ (TS _ 17)    }
  ':'       { PT _ (TS _ 18)    }
  ':='      { PT _ (TS _ 19)    }
  ';'       { PT _ (TS _ 20)    }
  '<'       { PT _ (TS _ 21)    }
  '<<'      { PT _ (TS _ 22)    }
  '<='      { PT _ (TS _ 23)    }
  '='       { PT _ (TS _ 24)    }
  '=='      { PT _ (TS _ 25)    }
  '>'       { PT _ (TS _ 26)    }
  '>='      { PT _ (TS _ 27)    }
  '>>'      { PT _ (TS _ 28)    }
  '@'       { PT _ (TS _ 29)    }
  '['       { PT _ (TS _ 30)    }
  ']'       { PT _ (TS _ 31)    }
  '^'       { PT _ (TS _ 32)    }
  'alignof' { PT _ (TS _ 33)    }
  'bool'    { PT _ (TS _ 34)    }
  'cast'    { PT _ (TS _ 35)    }
  'char'    { PT _ (TS _ 36)    }
  'elif'    { PT _ (TS _ 37)    }
  'else'    { PT _ (TS _ 38)    }
  'end'     { PT _ (TS _ 39)    }
  'f64'     { PT _ (TS _ 40)    }
  'false'   { PT _ (TS _ 41)    }
  'func'    { PT _ (TS _ 42)    }
  'i64'     { PT _ (TS _ 43)    }
  'if'      { PT _ (TS _ 44)    }
  'nil'     { PT _ (TS _ 45)    }
  'return'  { PT _ (TS _ 46)    }
  'sizeof'  { PT _ (TS _ 47)    }
  'struct'  { PT _ (TS _ 48)    }
  'true'    { PT _ (TS _ 49)    }
  'union'   { PT _ (TS _ 50)    }
  'void'    { PT _ (TS _ 51)    }
  'while'   { PT _ (TS _ 52)    }
  '|'       { PT _ (TS _ 53)    }
  '||'      { PT _ (TS _ 54)    }
  '~'       { PT _ (TS _ 55)    }
  L_charac  { PT _ (TC $$)      }
  L_doubl   { PT _ (TD $$)      }
  L_integ   { PT _ (TI $$)      }
  L_quoted  { PT _ (TL $$)      }
  L_IDENT   { PT _ (T_IDENT $$) }

%%

Char    :: { Char }
Char     : L_charac { (read (Data.Text.unpack $1)) :: Char }

Double  :: { Double }
Double   : L_doubl  { (read (Data.Text.unpack $1)) :: Double }

Integer :: { Integer }
Integer  : L_integ  { (read (Data.Text.unpack $1)) :: Integer }

String  :: { String }
String   : L_quoted { (Data.Text.unpack $1) }

IDENT :: { BNFC.Pristinum.Abs.IDENT }
IDENT  : L_IDENT { BNFC.Pristinum.Abs.IDENT $1 }

Program :: { BNFC.Pristinum.Abs.Program }
Program : ListProgramUnit { BNFC.Pristinum.Abs.PProgram $1 }

ProgramUnit :: { BNFC.Pristinum.Abs.ProgramUnit }
ProgramUnit
  : Record { BNFC.Pristinum.Abs.PURecord $1 }
  | Bind ':=' Expr ';' { BNFC.Pristinum.Abs.PUBind $1 $3 }
  | Bind ';' { BNFC.Pristinum.Abs.PUBindVoid $1 }
  | Function { BNFC.Pristinum.Abs.PUFunction $1 }

ListProgramUnit :: { [BNFC.Pristinum.Abs.ProgramUnit] }
ListProgramUnit
  : {- empty -} { [] } | ProgramUnit ListProgramUnit { (:) $1 $2 }

Record :: { BNFC.Pristinum.Abs.Record }
Record
  : RecordType IDENT ':' ListBind2 'end' { BNFC.Pristinum.Abs.RRecord $1 $2 $4 }

RecordType :: { BNFC.Pristinum.Abs.RecordType }
RecordType
  : 'struct' { BNFC.Pristinum.Abs.RTStruct }
  | 'union' { BNFC.Pristinum.Abs.RTUnion }

Function :: { BNFC.Pristinum.Abs.Function }
Function
  : 'func' IDENT '(' ListBind1 ')' '@' Type ':' ListStmt 'end' { BNFC.Pristinum.Abs.FFunction $2 $4 $7 $9 }

Stmt :: { BNFC.Pristinum.Abs.Stmt }
Stmt
  : Expr ';' { BNFC.Pristinum.Abs.SExpr $1 }
  | Bind ':=' Expr ';' { BNFC.Pristinum.Abs.SBind $1 $3 }
  | Bind ';' { BNFC.Pristinum.Abs.SBindVoid $1 }
  | 'if' Expr ':' ListStmt ListElifStmt 'end' { BNFC.Pristinum.Abs.SIf $2 $4 $5 }
  | 'if' Expr ':' ListStmt ListElifStmt 'else' ListStmt 'end' { BNFC.Pristinum.Abs.SIfElse $2 $4 $5 $7 }
  | 'while' Expr ':' ListStmt 'end' { BNFC.Pristinum.Abs.SWhile $2 $4 }
  | 'return' Expr ';' { BNFC.Pristinum.Abs.SReturn $2 }
  | 'return' ';' { BNFC.Pristinum.Abs.SReturnVoid }

ListStmt :: { [BNFC.Pristinum.Abs.Stmt] }
ListStmt : {- empty -} { [] } | Stmt ListStmt { (:) $1 $2 }

ElifStmt :: { BNFC.Pristinum.Abs.ElifStmt }
ElifStmt
  : 'elif' Expr ':' ListStmt { BNFC.Pristinum.Abs.ESElif $2 $4 }

ListElifStmt :: { [BNFC.Pristinum.Abs.ElifStmt] }
ListElifStmt
  : {- empty -} { [] } | ElifStmt ListElifStmt { (:) $1 $2 }

Expr :: { BNFC.Pristinum.Abs.Expr }
Expr
  : Expr1 '=' Expr { BNFC.Pristinum.Abs.EAssign $1 $3 }
  | Expr1 { $1 }

Expr1 :: { BNFC.Pristinum.Abs.Expr }
Expr1
  : Expr1 '||' Expr2 { BNFC.Pristinum.Abs.ELOr $1 $3 } | Expr2 { $1 }

Expr2 :: { BNFC.Pristinum.Abs.Expr }
Expr2
  : Expr2 '&&' Expr3 { BNFC.Pristinum.Abs.ELAnd $1 $3 }
  | Expr3 { $1 }

Expr3 :: { BNFC.Pristinum.Abs.Expr }
Expr3
  : Expr3 '|' Expr4 { BNFC.Pristinum.Abs.EBOr $1 $3 } | Expr4 { $1 }

Expr4 :: { BNFC.Pristinum.Abs.Expr }
Expr4
  : Expr4 '^' Expr5 { BNFC.Pristinum.Abs.EBXor $1 $3 } | Expr5 { $1 }

Expr5 :: { BNFC.Pristinum.Abs.Expr }
Expr5
  : Expr5 '&' Expr6 { BNFC.Pristinum.Abs.EBAnd $1 $3 } | Expr6 { $1 }

Expr6 :: { BNFC.Pristinum.Abs.Expr }
Expr6
  : Expr6 '==' Expr7 { BNFC.Pristinum.Abs.EEqual $1 $3 }
  | Expr6 '!=' Expr7 { BNFC.Pristinum.Abs.ENotEqual $1 $3 }
  | Expr7 { $1 }

Expr7 :: { BNFC.Pristinum.Abs.Expr }
Expr7
  : Expr7 '<' Expr8 { BNFC.Pristinum.Abs.ELess $1 $3 }
  | Expr7 '<=' Expr8 { BNFC.Pristinum.Abs.ELessEqual $1 $3 }
  | Expr7 '>' Expr8 { BNFC.Pristinum.Abs.EGreater $1 $3 }
  | Expr7 '>=' Expr8 { BNFC.Pristinum.Abs.EGreaterEqual $1 $3 }
  | Expr8 { $1 }

Expr8 :: { BNFC.Pristinum.Abs.Expr }
Expr8
  : Expr8 '<<' Expr9 { BNFC.Pristinum.Abs.EBShl $1 $3 }
  | Expr8 '>>' Expr9 { BNFC.Pristinum.Abs.EBShr $1 $3 }
  | Expr9 { $1 }

Expr9 :: { BNFC.Pristinum.Abs.Expr }
Expr9
  : Expr9 '+' Expr10 { BNFC.Pristinum.Abs.EAdd $1 $3 }
  | Expr9 '-' Expr10 { BNFC.Pristinum.Abs.ESubtract $1 $3 }
  | Expr10 { $1 }

Expr10 :: { BNFC.Pristinum.Abs.Expr }
Expr10
  : Expr10 '**' Expr11 { BNFC.Pristinum.Abs.EPower $1 $3 }
  | Expr10 '*' Expr11 { BNFC.Pristinum.Abs.EMultiply $1 $3 }
  | Expr10 '/' Expr11 { BNFC.Pristinum.Abs.EDivide $1 $3 }
  | Expr10 '%' Expr11 { BNFC.Pristinum.Abs.EMod $1 $3 }
  | Expr11 { $1 }

Expr11 :: { BNFC.Pristinum.Abs.Expr }
Expr11
  : '++' Expr11 { BNFC.Pristinum.Abs.EIncr $2 }
  | '--' Expr11 { BNFC.Pristinum.Abs.EDecr $2 }
  | '+' Expr11 { BNFC.Pristinum.Abs.EPos $2 }
  | '-' Expr11 { BNFC.Pristinum.Abs.ENeg $2 }
  | '!' Expr11 { BNFC.Pristinum.Abs.ELNot $2 }
  | '~' Expr11 { BNFC.Pristinum.Abs.EBNot $2 }
  | 'cast' '<' Type '>' '(' Expr ')' { BNFC.Pristinum.Abs.ECast $3 $6 }
  | '*' Expr11 { BNFC.Pristinum.Abs.EDeref $2 }
  | '&' Expr11 { BNFC.Pristinum.Abs.ERef $2 }
  | 'sizeof' '(' Type ')' { BNFC.Pristinum.Abs.ESizeof $3 }
  | 'alignof' '(' Type ')' { BNFC.Pristinum.Abs.EAlignof $3 }
  | Expr12 { $1 }

Expr12 :: { BNFC.Pristinum.Abs.Expr }
Expr12
  : Expr12 '++' { BNFC.Pristinum.Abs.EPIncr $1 }
  | Expr12 '--' { BNFC.Pristinum.Abs.EPDecr $1 }
  | IDENT '(' ListExpr ')' { BNFC.Pristinum.Abs.ECall $1 $3 }
  | Expr13 '[' Expr ']' { BNFC.Pristinum.Abs.EIndex $1 $3 }
  | Expr12 '.' Expr13 { BNFC.Pristinum.Abs.EAccess $1 $3 }
  | Expr12 '->' Expr13 { BNFC.Pristinum.Abs.EPAccess $1 $3 }
  | Expr13 { $1 }

Expr13 :: { BNFC.Pristinum.Abs.Expr }
Expr13
  : 'nil' { BNFC.Pristinum.Abs.ENil }
  | 'true' { BNFC.Pristinum.Abs.ETrue }
  | 'false' { BNFC.Pristinum.Abs.EFalse }
  | Char { BNFC.Pristinum.Abs.EChar $1 }
  | Integer { BNFC.Pristinum.Abs.EInt $1 }
  | Double { BNFC.Pristinum.Abs.EDouble $1 }
  | String { BNFC.Pristinum.Abs.EString $1 }
  | IDENT { BNFC.Pristinum.Abs.EIdent $1 }
  | '(' Expr ')' { $2 }

ListExpr :: { [BNFC.Pristinum.Abs.Expr] }
ListExpr
  : {- empty -} { [] }
  | Expr { (:[]) $1 }
  | Expr ',' ListExpr { (:) $1 $3 }

Bind :: { BNFC.Pristinum.Abs.Bind }
Bind : IDENT '@' Type { BNFC.Pristinum.Abs.BBind $1 $3 }

Bind1 :: { BNFC.Pristinum.Abs.Bind }
Bind1 : Bind { $1 }

Bind2 :: { BNFC.Pristinum.Abs.Bind }
Bind2 : Bind { $1 }

ListBind1 :: { [BNFC.Pristinum.Abs.Bind] }
ListBind1
  : {- empty -} { [] }
  | Bind1 { (:[]) $1 }
  | Bind1 ',' ListBind1 { (:) $1 $3 }

ListBind2 :: { [BNFC.Pristinum.Abs.Bind] }
ListBind2
  : Bind2 ';' { (:[]) $1 } | Bind2 ';' ListBind2 { (:) $1 $3 }

Type :: { BNFC.Pristinum.Abs.Type }
Type
  : 'void' { BNFC.Pristinum.Abs.TVoid }
  | 'bool' { BNFC.Pristinum.Abs.TBool }
  | 'char' { BNFC.Pristinum.Abs.TChar }
  | 'i64' { BNFC.Pristinum.Abs.TInt64 }
  | 'f64' { BNFC.Pristinum.Abs.TFloat64 }
  | Type '*' { BNFC.Pristinum.Abs.TPointer $1 }
  | IDENT { BNFC.Pristinum.Abs.TIdent $1 }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: Data.Text.Text -> [Token]
myLexer = tokens

}

