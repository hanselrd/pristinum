module Pristinum.AST where

import           Data.Int

data Expr
  = LNull
  | LBool Bool
  | LString String
  | LNumber Int64
  | Variable String
  | Call String [Expr]
  | Unary UnOp Expr
  | Binary BinOp Expr Expr
  deriving (Show, Eq)

data UnOp = Negative | BitwiseNot | LogicalNot deriving (Show, Eq)

data BinOp
  = Add
  | Subtract
  | Multiply
  | Divide
  | Mod
  | BitwiseShiftLeft
  | BitwiseShiftRight
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | Equal
  | NotEqual
  | Less
  | Greater
  | LessEqual
  | GreaterEqual
  | LogicalAnd
  | LogicalOr
  deriving (Show, Eq)

data Stmt
  = ExprStmt Expr
  | LetStmt String Expr
  | AssignStmt String Expr
  | IfStmt Expr [Stmt] (Maybe [Stmt])
  | WhileStmt Expr [Stmt]
  | FunctionStmt String [String] [Stmt]
  | ReturnStmt (Maybe Expr)
  deriving (Show, Eq)
