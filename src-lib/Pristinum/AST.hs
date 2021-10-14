module Pristinum.AST where

type Identifier = String

newtype Program = Program [Stmt]
  deriving (Show, Eq)

data Stmt
  = ExprStmt
      { exprStmtBody :: Expr
      }
  | LetStmt
      { letStmtName :: Identifier,
        letStmtBody :: Expr
      }
  | AssignStmt
      { assignStmtName :: Identifier,
        assignStmtBody :: Expr
      }
  | IfStmt
      { ifStmtCondition :: Expr,
        ifStmtBody :: [Stmt],
        ifStmtElseBody :: Maybe [Stmt]
      }
  | WhileStmt
      { whileStmtCondition :: Expr,
        whileStmtBody :: [Stmt]
      }
  | FunctionStmt
      { functionStmtName :: Identifier,
        functionStmtParameters :: [Identifier],
        functionStmtBody :: [Stmt]
      }
  | ReturnStmt
      { returnStmtBody :: Maybe Expr
      }
  deriving (Show, Eq)

data Expr
  = ExprNull
  | ExprBool Bool
  | ExprNumber Integer
  | ExprString String
  | ExprVariable Identifier
  | ExprCall Identifier [Expr]
  | ExprUnaryOp UnOp Expr
  | ExprBinaryOp BinOp Expr Expr
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
