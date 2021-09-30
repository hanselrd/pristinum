module Pristinum.Parser (parseString) where

import           Pristinum.AST
import           Pristinum.Lexer
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr

expr = buildExpressionParser operators term

operators =
  [ [ Prefix (reservedOp "-" >> return (Unary Negative))
    , Prefix (reservedOp "!" >> return (Unary LogicalNot))
    , Prefix (reservedOp "~" >> return (Unary BitwiseNot))
    ]
  , [ Infix (reservedOp "*" >> return (Binary Multiply)) AssocLeft
    , Infix (reservedOp "/" >> return (Binary Divide))   AssocLeft
    , Infix (reservedOp "%" >> return (Binary Mod))      AssocLeft
    ]
  , [ Infix (reservedOp "+" >> return (Binary Add))      AssocLeft
    , Infix (reservedOp "-" >> return (Binary Subtract)) AssocLeft
    ]
  , [ Infix (reservedOp "<<" >> return (Binary BitwiseShiftLeft))  AssocLeft
    , Infix (reservedOp ">>" >> return (Binary BitwiseShiftRight)) AssocLeft
    ]
  , [ Infix (reservedOp "<" >> return (Binary Less))          AssocLeft
    , Infix (reservedOp "<=" >> return (Binary LessEqual))    AssocLeft
    , Infix (reservedOp ">" >> return (Binary Greater))       AssocLeft
    , Infix (reservedOp ">=" >> return (Binary GreaterEqual)) AssocLeft
    ]
  , [ Infix (reservedOp "==" >> return (Binary Equal))    AssocLeft
    , Infix (reservedOp "!=" >> return (Binary NotEqual)) AssocLeft
    ]
  , [Infix (reservedOp "&" >> return (Binary BitwiseAnd)) AssocLeft]
  , [Infix (reservedOp "^" >> return (Binary BitwiseXor)) AssocLeft]
  , [Infix (reservedOp "|" >> return (Binary BitwiseOr)) AssocLeft]
  , [Infix (reservedOp "&&" >> return (Binary LogicalAnd)) AssocLeft]
  , [Infix (reservedOp "&&" >> return (Binary LogicalOr)) AssocLeft]
  ]

term =
  LNull
    <$  reserved "null"
    <|> LBool True
    <$  reserved "true"
    <|> LBool False
    <$  reserved "false"
    <|> LString
    <$> stringLiteral
    <|> LNumber
    .   fromIntegral
    <$> natural
    <|> try (Call <$> identifier <*> parens (sepBy expr (comma *> whiteSpace)))
    <|> Variable
    <$> identifier
    <|> parens expr

stmt =
  LetStmt
    <$> (reserved "let" *> identifier)
    <*> (reservedOp "=" *> expr)
    <|> AssignStmt
    <$> identifier
    <*> (reservedOp "=" *> expr)
    <|> IfStmt
    <$> (reserved "if" *> expr <* (char ':' *> whiteSpace))
    <*> many stmt
    <*> (   (reserved "else" *> (Just <$> many stmt) <* reserved "end")
        <|> (Nothing <$ reserved "end")
        )
    <|> WhileStmt
    <$> (reserved "while" *> expr <* (char ':' *> whiteSpace))
    <*> (many stmt <* reserved "end")
    <|> FunctionStmt
    <$> (reservedOp "@" *> identifier)
    <*> parens (sepBy identifier (comma *> whiteSpace))
    <*> ((char ':' *> whiteSpace) *> many stmt <* reserved "end")
    <|> ReturnStmt
    <$> (reserved "return" *> ((Just <$> expr) <|> return Nothing))
    <|> ExprStmt
    <$> expr

parser = whiteSpace *> many stmt <* eof

parseString = parse parser ""

{-
Language Example:
Pristinum

import intrinsics as I

@main (argc, argv):
    let x = 10
    I.print(x)
    if x >= 10:
        I.print(x)
    end
end
-}
