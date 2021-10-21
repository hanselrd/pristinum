module Pristinum.AST where

import Data.Text hiding
  ( head,
    length,
    map,
    null,
    tail,
  )
import Prettyprinter

newtype Program = Program [Stmt]
  deriving (Show, Eq)

data Stmt
  = ExprStmt Expr
  | BindStmt Bind Expr
  | AssignStmt Text Expr
  | IfStmt [(Expr, [Stmt])] (Maybe [Stmt])
  | WhileStmt Expr [Stmt]
  | ReturnStmt (Maybe Expr)
  | FunctionStmt
      { functionStmtReturnType :: Type,
        functionStmtName :: Text,
        functionStmtParameters :: [Bind],
        functionStmtBody :: [Stmt]
      }
  | StructStmt
      { structStmtName :: Text,
        structStmtFields :: [Bind]
      }
  deriving (Show, Eq)

data Expr
  = ExprNil
  | ExprBool Bool
  | ExprChar Char
  | ExprInt Int
  | ExprFloat Double
  | ExprString Text
  | ExprVariable Text
  | ExprUnaryOp UnOp Expr
  | ExprBinaryOp BinOp Expr Expr
  | ExprCall Text [Expr]
  | ExprCast Type Expr
  | ExprAccess Expr Expr
  | ExprDeref Expr
  | ExprRef Expr
  | ExprSizeof Type
  | ExprNop
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
  | BinOpPower
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

data Bind = Bind
  { bindType :: Type,
    bindName :: Text
  }
  deriving (Show, Eq)

data Type
  = TypeVoid
  | TypeBool
  | TypeChar
  | TypeInt8
  | TypeInt16
  | TypeInt32
  | TypeInt64
  | TypeUint8
  | TypeUint16
  | TypeUint32
  | TypeUint64
  | TypeFloat32
  | TypeFloat64
  | TypePointer Type
  | TypeStruct Text
  deriving (Show, Eq)

instance Pretty Program where
  pretty = \case
    Program sts -> concatWith (\x y -> x <> hardline <> y) (map pretty sts)

instance Pretty Stmt where
  pretty = \case
    ExprStmt ex -> pretty ex <+> "."
    BindStmt bi ex -> pretty bi <+> ":=" <+> pretty ex <+> "."
    AssignStmt txt ex -> pretty txt <+> "=" <+> pretty ex <+> "."
    IfStmt x0 m_sts ->
      "if"
        <+> pretty (fst (head x0))
        <> ":"
        <> nest
          4
          ( if not (null (snd (head x0)))
              then hardline <> vsep (map pretty (snd (head x0)))
              else mempty
          )
        <> (if not (null (tail x0)) then hardline else mempty)
        <> vsep
          ( map
              ( \(cond, body) ->
                  "elif" <+> pretty cond <> ":"
                    <> nest
                      4
                      ( if not (null body)
                          then hardline <> vsep (map pretty body)
                          else mempty
                      )
              )
              (tail x0)
          )
        <> maybe
          mempty
          ( \sts ->
              hardline <> "else"
                <> nest
                  4
                  ( if not (null sts)
                      then hardline <> vsep (map pretty sts)
                      else mempty
                  )
          )
          m_sts
        <> hardline
        <> "."
    WhileStmt ex sts ->
      "while"
        <+> pretty ex
        <> ":"
        <> nest
          4
          ( if not (null sts)
              then hardline <> vsep (map pretty sts)
              else mempty
          )
        <> hardline
        <> "."
    ReturnStmt m_ex ->
      "return"
        <+> ( case m_ex of
                Nothing -> "."
                Just ex -> pretty ex <+> "."
            )
    FunctionStmt ty txt bis sts ->
      "func"
        <+> pretty txt
        <> tupled (map pretty bis)
        <+> "@"
        <> pretty ty
        <> ":"
        <> nest
          4
          ( if not (null sts)
              then hardline <> vsep (map pretty sts)
              else mempty
          )
        <> hardline
        <> "."
    StructStmt txt bis ->
      "struct"
        <+> pretty txt
        <> ":"
        <> nest
          4
          ( if not (null bis)
              then hardline <> vsep (map (\b -> pretty b <+> ".") bis)
              else mempty
          )
        <> hardline
        <> "."

instance Pretty Expr where
  pretty = \case
    ExprNil -> "nil"
    ExprBool b -> if b then "true" else "false"
    ExprChar c -> squotes $ pretty c
    ExprInt i -> pretty i
    ExprFloat f -> pretty f
    ExprString s -> dquotes $ pretty s
    ExprVariable txt -> pretty txt
    ExprUnaryOp uo ex -> pretty ex <+> pretty uo
    ExprBinaryOp bo ex1 ex2 -> pretty ex1 <+> pretty ex2 <+> pretty bo
    ExprCall txt exs -> pretty txt <> tupled (map pretty exs)
    ExprCast ty ex -> angles (pretty ty) <> parens (pretty ex) -- pretty ex <+> "#" <> pretty ty
    ExprAccess ex1 ex2 -> pretty ex1 <> "#" <> pretty ex2
    ExprDeref ex -> "&" <> parens (pretty ex)
    ExprRef ex -> "*" <> parens (pretty ex)
    ExprSizeof ty -> "sizeof" <> parens (pretty ty)
    ExprNop -> mempty

instance Pretty UnOp where
  pretty = \case
    UnOpNegative -> "-"
    UnOpBitwiseNot -> "~"
    UnOpLogicalNot -> "!"

instance Pretty BinOp where
  pretty = \case
    BinOpAdd -> "+"
    BinOpSubtract -> "-"
    BinOpMultiply -> "*"
    BinOpDivide -> "/"
    BinOpMod -> "%"
    BinOpPower -> "**"
    BinOpBitwiseShiftLeft -> "<<"
    BinOpBitwiseShiftRight -> ">>"
    BinOpBitwiseAnd -> "&"
    BinOpBitwiseOr -> "|"
    BinOpBitwiseXor -> "^"
    BinOpEqual -> "=="
    BinOpNotEqual -> "!="
    BinOpLess -> "<"
    BinOpGreater -> ">"
    BinOpLessEqual -> "<="
    BinOpGreaterEqual -> ">="
    BinOpLogicalAnd -> "&&"
    BinOpLogicalOr -> "||"

instance Pretty Bind where
  pretty (Bind ty txt) = pretty txt <+> "@" <> pretty ty

instance Pretty Type where
  pretty = \case
    TypeVoid -> "void"
    TypeBool -> "bool"
    TypeChar -> "char"
    TypeInt8 -> "i8"
    TypeInt16 -> "i16"
    TypeInt32 -> "i32"
    TypeInt64 -> "i64"
    TypeUint8 -> "u8"
    TypeUint16 -> "u16"
    TypeUint32 -> "u32"
    TypeUint64 -> "u64"
    TypeFloat32 -> "f32"
    TypeFloat64 -> "f64"
    TypePointer ty -> pretty ty <+> "*"
    TypeStruct txt -> "struct" <+> pretty txt
