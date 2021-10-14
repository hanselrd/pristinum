module Pristinum.Generator.C where

import Data.Bits
import Data.List
import Pristinum.AST

generateC (Program stmts) = unlines (map generateCStmt stmts)

generateCStmt stmt = case stmt of
  ExprStmt body -> generateCExpr body ++ ";"
  LetStmt name body -> "void*" ++ name ++ "=" ++ generateCExpr body ++ ";"
  AssignStmt name body -> name ++ "=" ++ generateCExpr body ++ ";"
  IfStmt cond body mElseBody ->
    "if("
      ++ generateCExpr cond
      ++ ")"
      ++ "{"
      ++ unlines (map generateCStmt body)
      ++ "}"
      ++ ( case mElseBody of
             Nothing -> ""
             Just elseBody ->
               "else{" ++ unlines (map generateCStmt elseBody) ++ "}"
         )
  WhileStmt cond body ->
    "while("
      ++ generateCExpr cond
      ++ ")"
      ++ "{"
      ++ unlines (map generateCStmt body)
      ++ "}"
  FunctionStmt name params body ->
    "int "
      ++ "pristinum_"
      ++ name
      ++ "("
      ++ intercalate "," params
      ++ ")"
      ++ "{"
      ++ unlines (map generateCStmt body)
      ++ "}"
  ReturnStmt mBody -> "return(" ++ maybe "" generateCExpr mBody ++ ");"

generateCExpr expr = case expr of
  ExprNull -> "NULL"
  ExprBool b -> if b then "true" else "false"
  ExprNumber n -> show n
  ExprString s -> "\"" ++ s ++ "\""
  ExprVariable v -> v
  ExprCall c params ->
    c ++ "(" ++ intercalate "," (map generateCExpr params) ++ ")"
  ExprUnaryOp op l ->
    "("
      ++ ( case op of
             UnOpNegative -> "-"
             UnOpBitwiseNot -> "~"
             UnOpLogicalNot -> "!"
         )
      ++ generateCExpr l
      ++ ")"
  ExprBinaryOp op l r ->
    "("
      ++ generateCExpr l
      ++ ( case op of
             BinOpAdd -> "+"
             BinOpSubtract -> "-"
             BinOpMultiply -> "*"
             BinOpDivide -> "/"
             BinOpMod -> "%"
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
         )
      ++ generateCExpr r
      ++ ")"

reduceCExpr expr = case expr of
  unary@ExprUnaryOp {} -> reduceCUnaryOpExpr unary
  binary@ExprBinaryOp {} -> reduceCBinaryOpExpr binary
  _ -> expr

reduceCUnaryOpExpr expr@(~(ExprUnaryOp op e)) = case (op, e) of
  (UnOpNegative, ExprNumber n) -> ExprNumber (negate n)
  (UnOpBitwiseNot, ExprNumber n) -> ExprNumber (complement n)
  (UnOpLogicalNot, ExprBool b) -> ExprBool (not b)
  _ -> expr

reduceCBinaryOpExpr expr@(~(ExprBinaryOp op e1 e2)) = case (op, e1, e2) of
  (BinOpAdd, ExprNumber n1, ExprNumber n2) -> ExprNumber (n1 + n2)
  (BinOpAdd, ExprString s1, ExprString s2) -> ExprString (s1 <> s2)
  (BinOpAdd, ExprString s1, _) -> ExprString (s1 <> show e1)
  (BinOpAdd, _, ExprString s2) -> ExprString (show e2 <> s2)
  (BinOpSubtract, ExprNumber n1, ExprNumber n2) -> ExprNumber (n1 - n2)
  (BinOpMultiply, ExprNumber n1, ExprNumber n2) -> ExprNumber (n1 * n2)
  (BinOpDivide, ExprNumber n1, ExprNumber n2) -> ExprNumber (n1 `div` n2)
  (BinOpMod, ExprNumber n1, ExprNumber n2) -> ExprNumber (n1 `mod` n2)
  (BinOpBitwiseShiftLeft, ExprNumber n1, ExprNumber n2) ->
    ExprNumber (n1 `shiftL` fromInteger n2)
  (BinOpBitwiseShiftRight, ExprNumber n1, ExprNumber n2) ->
    ExprNumber (n1 `shiftR` fromInteger n2)
  (BinOpBitwiseAnd, ExprNumber n1, ExprNumber n2) -> ExprNumber (n1 .&. n2)
  (BinOpBitwiseOr, ExprNumber n1, ExprNumber n2) -> ExprNumber (n1 .|. n2)
  (BinOpBitwiseXor, ExprNumber n1, ExprNumber n2) -> ExprNumber (n1 `xor` n2)
  (BinOpEqual, _, _) -> ExprBool (e1 == e1)
  (BinOpNotEqual, _, _) -> ExprBool (e1 /= e2)
  (BinOpLess, ExprNumber n1, ExprNumber n2) -> ExprBool (n1 < n2)
  (BinOpGreater, ExprNumber n1, ExprNumber n2) -> ExprBool (n1 > n2)
  (BinOpLessEqual, ExprNumber n1, ExprNumber n2) -> ExprBool (n1 <= n2)
  (BinOpGreaterEqual, ExprNumber n1, ExprNumber n2) -> ExprBool (n1 >= n2)
  (BinOpLogicalAnd, ExprBool b1, ExprBool b2) -> ExprBool (b1 && b2)
  (BinOpLogicalOr, ExprBool b1, ExprBool b2) -> ExprBool (b1 || b2)
  _ -> expr
