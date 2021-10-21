-- File generated by the BNF Converter (bnfc 2.9.3).
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language Pristinum.
module Codegen.Pristinum.Abs where

import qualified Data.String
import qualified Data.Text
import Prelude
  ( Char,
    Double,
    Integer,
    String,
  )
import qualified Prelude as C
  ( Eq,
    Ord,
    Read,
    Show,
  )

data Program = PProgram [Stmt]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Stmt
  = SExpr Expr
  | SBind Bind Expr
  | SAssign IDENT Expr
  | SIf Expr [Stmt] [ElifStmt]
  | SIfElse Expr [Stmt] [ElifStmt] [Stmt]
  | SWhile Expr [Stmt]
  | SReturn Expr
  | SReturnVoid
  | SFunction IDENT [Bind] Type [Stmt]
  | SStruct IDENT [Bind]
  | SUnion IDENT [Bind]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ElifStmt = ESElif Expr [Stmt]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Expr
  = ENil
  | ETrue
  | EFalse
  | EChar Char
  | EInt Integer
  | EDouble Double
  | EString String
  | EIdent IDENT
  | ECall IDENT [Expr]
  | ESizeof Type
  | EDecr Expr
  | EIncr Expr
  | EBitNot Expr
  | ENot Expr
  | EAdd Expr Expr
  | ESubtract Expr Expr
  | EMultiply Expr Expr
  | EDivide Expr Expr
  | EMod Expr Expr
  | EPower Expr Expr
  | EBitShl Expr Expr
  | EBitShr Expr Expr
  | EBitAnd Expr Expr
  | EBitOr Expr Expr
  | EBitXor Expr Expr
  | EEqual Expr Expr
  | ENotEqual Expr Expr
  | ELess Expr Expr
  | EGreater Expr Expr
  | ELessEqual Expr Expr
  | EGreaterEqual Expr Expr
  | EAnd Expr Expr
  | EOr Expr Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Bind = BBind IDENT Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Type
  = TVoid
  | TBool
  | TChar
  | TInt8
  | TInt16
  | TInt32
  | TInt64
  | TUint8
  | TUint16
  | TUint32
  | TUint64
  | TFloat32
  | TFloat64
  | TPointer Type
  | TStruct IDENT
  | TUnion IDENT
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype IDENT = IDENT Data.Text.Text
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)