module Pristinum.Lexer
  ( identifier
  , reserved
  , reservedOp
  , natural
  , stringLiteral
  , parens
  , comma
  , whiteSpace
  ) where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token

languageDef = emptyDef
  { Token.commentStart    = "/*"
  , Token.commentEnd      = "*/"
  , Token.commentLine     = "//"
  , Token.identStart      = letter <|> char '_'
  , Token.identLetter     = alphaNum <|> char '_'
  , Token.reservedNames   = [ "let"
                            , "if"
                            , "else"
                            , "while"
                            , "end"
                            , "return"
                            , "null"
                            , "true"
                            , "false"
                            , "mem"
                            , "load"
                            , "store"
                            , "print"
                            ]
  , Token.reservedOpNames = [ "@"
                            , ":"
                            , "="
                            , ","
                            , "+"
                            , "-"
                            , "*"
                            , "/"
                            , "%"
                            , "<<"
                            , ">>"
                            , "=="
                            , "!="
                            , "<"
                            , ">"
                            , "<="
                            , ">="
                            , "&"
                            , "|"
                            , "^"
                            , "&&"
                            , "||"
                            , "!"
                            , "~"
                            ]
  }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer

reserved = Token.reserved lexer

reservedOp = Token.reservedOp lexer

natural = Token.natural lexer

stringLiteral = Token.stringLiteral lexer

parens = Token.parens lexer

comma = Token.comma lexer

whiteSpace = Token.whiteSpace lexer
