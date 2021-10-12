module Pristinum.ParserTest
  ( tests,
  )
where

import Pristinum.AST
import Pristinum.Parser
import Test.Tasty
import Test.Tasty.HUnit

unitTests =
  [ testGroup
      "expressions"
      [ testCase
          "null"
          (parseString "null" @?= Right (Program [PushStmt LitNull])),
        testCase
          "true"
          (parseString "true" @?= Right (Program [PushStmt (LitBool True)])),
        testCase
          "false"
          (parseString "false" @?= Right (Program [PushStmt (LitBool False)])),
        testCase
          "number"
          (parseString "123" @?= Right (Program [PushStmt (LitNumber 123)])),
        testCase
          "string"
          ( parseString "\"pristinum\""
              @?= Right (Program [PushStmt (LitString "pristinum")])
          ),
        testCase
          "identifier"
          (parseString "x" @?= Right (Program [PushStmt (LitIdentifier "x")])),
        testCase "drop" (parseString "drop" @?= Right (Program [DropStmt])),
        testGroup
          "unary operation"
          [ testCase
              "negative"
              (parseString "neg" @?= Right (Program [UnaryOpStmt UnOpNegative])),
            testCase
              "bitwise not"
              (parseString "~" @?= Right (Program [UnaryOpStmt UnOpBitwiseNot])),
            testCase
              "logical not"
              (parseString "!" @?= Right (Program [UnaryOpStmt UnOpLogicalNot]))
          ],
        testGroup
          "binary operation"
          [ testCase
              "add"
              (parseString "+" @?= Right (Program [BinaryOpStmt BinOpAdd])),
            testCase
              "subtract"
              (parseString "-" @?= Right (Program [BinaryOpStmt BinOpSubtract])),
            testCase
              "multiply"
              (parseString "*" @?= Right (Program [BinaryOpStmt BinOpMultiply])),
            testCase
              "divide"
              (parseString "/" @?= Right (Program [BinaryOpStmt BinOpDivide])),
            testCase
              "mod"
              (parseString "%" @?= Right (Program [BinaryOpStmt BinOpMod])),
            testCase
              "bitwise shift left"
              ( parseString "<<"
                  @?= Right (Program [BinaryOpStmt BinOpBitwiseShiftLeft])
              ),
            testCase
              "bitwise shift right"
              ( parseString ">>"
                  @?= Right (Program [BinaryOpStmt BinOpBitwiseShiftRight])
              ),
            testCase
              "bitwise and"
              (parseString "&" @?= Right (Program [BinaryOpStmt BinOpBitwiseAnd])),
            testCase
              "bitwise or"
              (parseString "|" @?= Right (Program [BinaryOpStmt BinOpBitwiseOr])),
            testCase
              "bitwise xor"
              (parseString "^" @?= Right (Program [BinaryOpStmt BinOpBitwiseXor])),
            testCase
              "equal"
              (parseString "==" @?= Right (Program [BinaryOpStmt BinOpEqual])),
            testCase
              "not equal"
              (parseString "!=" @?= Right (Program [BinaryOpStmt BinOpNotEqual])),
            testCase
              "less"
              (parseString "<" @?= Right (Program [BinaryOpStmt BinOpLess])),
            testCase
              "greater"
              (parseString ">" @?= Right (Program [BinaryOpStmt BinOpGreater])),
            testCase
              "less equal"
              (parseString "<=" @?= Right (Program [BinaryOpStmt BinOpLessEqual])),
            testCase
              "greater equal"
              ( parseString ">=" @?= Right (Program [BinaryOpStmt BinOpGreaterEqual])
              ),
            testCase
              "logical and"
              (parseString "&&" @?= Right (Program [BinaryOpStmt BinOpLogicalAnd])),
            testCase
              "logical or"
              (parseString "||" @?= Right (Program [BinaryOpStmt BinOpLogicalOr]))
          ],
        testGroup
          "if"
          [ testCase
              "without elif/else"
              ( parseString "if 1 do 2 end"
                  @?= Right
                    ( Program
                        [ IfStmt
                            [PushStmt (LitNumber 1)]
                            [PushStmt (LitNumber 2)]
                            []
                        ]
                    )
              ),
            testCase
              "without elif/with else"
              ( parseString "if 1 do 2 else 3 end"
                  @?= Right
                    ( Program
                        [ IfStmt
                            [PushStmt (LitNumber 1)]
                            [PushStmt (LitNumber 2)]
                            [PushStmt (LitNumber 3)]
                        ]
                    )
              ),
            testCase
              "with elif/without else"
              ( parseString "if 1 do 2 elif 3 do 4 end"
                  @?= Right
                    ( Program
                        [ IfStmt
                            [PushStmt (LitNumber 1)]
                            [PushStmt (LitNumber 2)]
                            [IfStmt [PushStmt (LitNumber 3)] [PushStmt (LitNumber 4)] []]
                        ]
                    )
              ),
            testCase
              "with elif/else"
              ( parseString "if 1 do 2 elif 3 do 4 else 5 end"
                  @?= Right
                    ( Program
                        [ IfStmt
                            [PushStmt (LitNumber 1)]
                            [PushStmt (LitNumber 2)]
                            [ IfStmt
                                [PushStmt (LitNumber 3)]
                                [PushStmt (LitNumber 4)]
                                [PushStmt (LitNumber 5)]
                            ]
                        ]
                    )
              ),
            testCase
              "with elif/else (advanced)"
              ( parseString
                  "if 1 do 2 elif 3 do 4 elif 5 do 6 elif 7 do 8 else 9 if 10 do 11 end end if 12 do 13 elif 14 do 15 else 16 end"
                  @?= Right
                    ( Program
                        [ IfStmt
                            [PushStmt (LitNumber 1)]
                            [PushStmt (LitNumber 2)]
                            [ IfStmt
                                [PushStmt (LitNumber 3)]
                                [PushStmt (LitNumber 4)]
                                [ IfStmt
                                    [PushStmt (LitNumber 5)]
                                    [PushStmt (LitNumber 6)]
                                    [ IfStmt
                                        [PushStmt (LitNumber 7)]
                                        [PushStmt (LitNumber 8)]
                                        [ PushStmt (LitNumber 9),
                                          IfStmt
                                            [PushStmt (LitNumber 10)]
                                            [PushStmt (LitNumber 11)]
                                            []
                                        ]
                                    ]
                                ]
                            ],
                          IfStmt
                            [PushStmt (LitNumber 12)]
                            [PushStmt (LitNumber 13)]
                            [ IfStmt
                                [PushStmt (LitNumber 14)]
                                [PushStmt (LitNumber 15)]
                                [PushStmt (LitNumber 16)]
                            ]
                        ]
                    )
              )
          ],
        testCase
          "while"
          ( parseString "while 1 do 2 end"
              @?= Right
                ( Program
                    [WhileStmt [PushStmt (LitNumber 1)] [PushStmt (LitNumber 2)]]
                )
          ),
        testCase
          "macro"
          ( parseString "macro test do 1 end"
              @?= Right (Program [MacroStmt "test" [PushStmt (LitNumber 1)]])
          )
      ]
  ]

tests = testGroup "Parser" unitTests
