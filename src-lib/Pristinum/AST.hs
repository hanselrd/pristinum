module Pristinum.AST where

import Data.Int

newtype Program = Program [Stmt]
  deriving (Show, Eq)

data Stmt
  = PushStmt {pushStmtLit :: Lit}
  | DropStmt
  | UnaryOpStmt {unaryOpStmtUnOp :: UnOp}
  | BinaryOpStmt {binaryOpStmtBinOp :: BinOp}
  | IfStmt
      { ifStmtCondition :: [Stmt],
        ifStmtBody :: [Stmt],
        ifStmtElseBody :: [Stmt]
      }
  | WhileStmt
      { whileStmtCondition :: [Stmt],
        whileStmtBody :: [Stmt]
      }
  | MacroStmt
      { macroStmtIdentifier :: String,
        macroStmtBody :: [Stmt]
      }
  deriving (Show, Eq)

data Lit
  = LitNull
  | LitBool Bool
  | LitNumber Integer
  | LitString String
  | LitIdentifier String
  deriving (Show, Eq)

data UnOp
  = UnOpNegative
  | UnOpBitwiseNot
  | UnOpLogicalNot
  deriving (Show, Eq)

data BinOp
  = BinOpAdd
  | BinOpSubtract
  | BinOpMultiply
  | BinOpDivide
  | BinOpMod
  | BinOpBitwiseShiftLeft
  | BinOpBitwiseShiftRight
  | BinOpBitwiseAnd
  | BinOpBitwiseOr
  | BinOpBitwiseXor
  | BinOpEqual
  | BinOpNotEqual
  | BinOpLess
  | BinOpGreater
  | BinOpLessEqual
  | BinOpGreaterEqual
  | BinOpLogicalAnd
  | BinOpLogicalOr
  deriving (Show, Eq)
