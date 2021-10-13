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
          (parseString "null ." @?= Right (Program [ExprStmt ExprNull])),
        testCase
          "true"
          (parseString "true ." @?= Right (Program [ExprStmt (ExprBool True)])),
        testCase
          "false"
          (parseString "false ." @?= Right (Program [ExprStmt (ExprBool False)])),
        testCase
          "number"
          (parseString "123 ." @?= Right (Program [ExprStmt (ExprNumber 123)])),
        testCase
          "string"
          ( parseString "\"pristinum\" ."
              @?= Right (Program [ExprStmt (ExprString "pristinum")])
          ),
        testCase
          "variable"
          (parseString "x ." @?= Right (Program [ExprStmt (ExprVariable "x")])),
        testGroup
          "call"
          [ testCase
              "with no arguments"
              ( parseString "x() ."
                  @?= Right (Program [ExprStmt {exprStmtBody = ExprCall "x" []}])
              ),
            testCase
              "with 1 argument"
              ( parseString "x(1) ."
                  @?= Right
                    ( Program
                        [ExprStmt {exprStmtBody = ExprCall "x" [ExprNumber 1]}]
                    )
              ),
            testCase
              "with 2 arguments"
              ( parseString "x(1, 2) ."
                  @?= Right
                    ( Program
                        [ ExprStmt
                            { exprStmtBody = ExprCall "x" [ExprNumber 1, ExprNumber 2]
                            }
                        ]
                    )
              )
          ],
        testGroup
          "unary operation"
          [ -- testCase
            --   "negative"
            --   ( parseString "1 neg ."
            --       @?= Right
            --         (Program [ExprStmt (ExprUnaryOp UnOpNegative (ExprNumber 1))])
            --   ),
            testCase
              "bitwise not"
              ( parseString "1 ~ ."
                  @?= Right
                    (Program [ExprStmt (ExprUnaryOp UnOpBitwiseNot (ExprNumber 1))])
              ),
            testCase
              "logical not"
              ( parseString "true ! ."
                  @?= Right
                    (Program [ExprStmt (ExprUnaryOp UnOpLogicalNot (ExprBool True))])
              )
          ],
        testGroup
          "binary operation"
          [ testCase
              "add"
              ( parseString "1 2 + ."
                  @?= Right
                    ( Program
                        [ ExprStmt
                            { exprStmtBody =
                                ExprBinaryOp
                                  BinOpAdd
                                  (ExprNumber 1)
                                  (ExprNumber 2)
                            }
                        ]
                    )
              ),
            testCase
              "subtract"
              ( parseString "1 2 - ."
                  @?= Right
                    ( Program
                        [ ExprStmt
                            { exprStmtBody =
                                ExprBinaryOp
                                  BinOpSubtract
                                  (ExprNumber 1)
                                  (ExprNumber 2)
                            }
                        ]
                    )
              ),
            testCase
              "multiply"
              ( parseString "1 2 * ."
                  @?= Right
                    ( Program
                        [ ExprStmt
                            { exprStmtBody =
                                ExprBinaryOp
                                  BinOpMultiply
                                  (ExprNumber 1)
                                  (ExprNumber 2)
                            }
                        ]
                    )
              ),
            testCase
              "divide"
              ( parseString "1 2 / ."
                  @?= Right
                    ( Program
                        [ ExprStmt
                            { exprStmtBody =
                                ExprBinaryOp
                                  BinOpDivide
                                  (ExprNumber 1)
                                  (ExprNumber 2)
                            }
                        ]
                    )
              ),
            testCase
              "mod"
              ( parseString "1 2 % ."
                  @?= Right
                    ( Program
                        [ ExprStmt
                            { exprStmtBody =
                                ExprBinaryOp
                                  BinOpMod
                                  (ExprNumber 1)
                                  (ExprNumber 2)
                            }
                        ]
                    )
              ),
            testCase
              "bitwise shift left"
              ( parseString "1 2 << ."
                  @?= Right
                    ( Program
                        [ ExprStmt
                            { exprStmtBody =
                                ExprBinaryOp
                                  BinOpBitwiseShiftLeft
                                  (ExprNumber 1)
                                  (ExprNumber 2)
                            }
                        ]
                    )
              ),
            testCase
              "bitwise shift right"
              ( parseString "1 2 >> ."
                  @?= Right
                    ( Program
                        [ ExprStmt
                            { exprStmtBody =
                                ExprBinaryOp
                                  BinOpBitwiseShiftRight
                                  (ExprNumber 1)
                                  (ExprNumber 2)
                            }
                        ]
                    )
              ),
            testCase
              "bitwise and"
              ( parseString "1 2 & ."
                  @?= Right
                    ( Program
                        [ ExprStmt
                            { exprStmtBody =
                                ExprBinaryOp
                                  BinOpBitwiseAnd
                                  (ExprNumber 1)
                                  (ExprNumber 2)
                            }
                        ]
                    )
              ),
            testCase
              "bitwise or"
              ( parseString "1 2 | ."
                  @?= Right
                    ( Program
                        [ ExprStmt
                            { exprStmtBody =
                                ExprBinaryOp
                                  BinOpBitwiseOr
                                  (ExprNumber 1)
                                  (ExprNumber 2)
                            }
                        ]
                    )
              ),
            testCase
              "bitwise xor"
              ( parseString "1 2 ^ ."
                  @?= Right
                    ( Program
                        [ ExprStmt
                            { exprStmtBody =
                                ExprBinaryOp
                                  BinOpBitwiseXor
                                  (ExprNumber 1)
                                  (ExprNumber 2)
                            }
                        ]
                    )
              ),
            testCase
              "equal"
              ( parseString "1 2 == ."
                  @?= Right
                    ( Program
                        [ ExprStmt
                            { exprStmtBody =
                                ExprBinaryOp
                                  BinOpEqual
                                  (ExprNumber 1)
                                  (ExprNumber 2)
                            }
                        ]
                    )
              ),
            testCase
              "not equal"
              ( parseString "1 2 != ."
                  @?= Right
                    ( Program
                        [ ExprStmt
                            { exprStmtBody =
                                ExprBinaryOp
                                  BinOpNotEqual
                                  (ExprNumber 1)
                                  (ExprNumber 2)
                            }
                        ]
                    )
              ),
            testCase
              "less"
              ( parseString "1 2 < ."
                  @?= Right
                    ( Program
                        [ ExprStmt
                            { exprStmtBody =
                                ExprBinaryOp
                                  BinOpLess
                                  (ExprNumber 1)
                                  (ExprNumber 2)
                            }
                        ]
                    )
              ),
            testCase
              "greater"
              ( parseString "1 2 > ."
                  @?= Right
                    ( Program
                        [ ExprStmt
                            { exprStmtBody =
                                ExprBinaryOp
                                  BinOpGreater
                                  (ExprNumber 1)
                                  (ExprNumber 2)
                            }
                        ]
                    )
              ),
            testCase
              "less equal"
              ( parseString "1 2 <= ."
                  @?= Right
                    ( Program
                        [ ExprStmt
                            { exprStmtBody =
                                ExprBinaryOp
                                  BinOpLessEqual
                                  (ExprNumber 1)
                                  (ExprNumber 2)
                            }
                        ]
                    )
              ),
            testCase
              "greater equal"
              ( parseString "1 2 >= ."
                  @?= Right
                    ( Program
                        [ ExprStmt
                            { exprStmtBody =
                                ExprBinaryOp
                                  BinOpGreaterEqual
                                  (ExprNumber 1)
                                  (ExprNumber 2)
                            }
                        ]
                    )
              ),
            testCase
              "logical and"
              ( parseString "true false && ."
                  @?= Right
                    ( Program
                        [ ExprStmt
                            { exprStmtBody =
                                ExprBinaryOp
                                  BinOpLogicalAnd
                                  (ExprBool True)
                                  (ExprBool False)
                            }
                        ]
                    )
              ),
            testCase
              "logical or"
              ( parseString "true false || ."
                  @?= Right
                    ( Program
                        [ ExprStmt
                            { exprStmtBody =
                                ExprBinaryOp
                                  BinOpLogicalOr
                                  (ExprBool True)
                                  (ExprBool False)
                            }
                        ]
                    )
              )
          ],
        testGroup
          "if"
          [ testCase
              "without elif/else"
              ( parseString "if 1 do 2 . end"
                  @?= Right
                    ( Program
                        [ IfStmt
                            { ifStmtCondition = ExprNumber 1,
                              ifStmtBody = [ExprStmt {exprStmtBody = ExprNumber 2}],
                              ifStmtElseBody = Nothing
                            }
                        ]
                    )
              ),
            testCase
              "without elif/with else"
              ( parseString "if 1 do 2 . else 3 . end"
                  @?= Right
                    ( Program
                        [ IfStmt
                            { ifStmtCondition = ExprNumber 1,
                              ifStmtBody = [ExprStmt {exprStmtBody = ExprNumber 2}],
                              ifStmtElseBody =
                                Just
                                  [ExprStmt {exprStmtBody = ExprNumber 3}]
                            }
                        ]
                    )
              ),
            testCase
              "with elif/without else"
              ( parseString "if 1 do 2 . elif 3 do 4 . end"
                  @?= Right
                    ( Program
                        [ IfStmt
                            { ifStmtCondition = ExprNumber 1,
                              ifStmtBody = [ExprStmt {exprStmtBody = ExprNumber 2}],
                              ifStmtElseBody =
                                Just
                                  [ IfStmt
                                      { ifStmtCondition = ExprNumber 3,
                                        ifStmtBody =
                                          [ ExprStmt
                                              { exprStmtBody = ExprNumber 4
                                              }
                                          ],
                                        ifStmtElseBody = Nothing
                                      }
                                  ]
                            }
                        ]
                    )
              ),
            testCase
              "with elif/else"
              ( parseString "if 1 do 2 . elif 3 do 4 . else 5 . end"
                  @?= Right
                    ( Program
                        [ IfStmt
                            { ifStmtCondition = ExprNumber 1,
                              ifStmtBody = [ExprStmt {exprStmtBody = ExprNumber 2}],
                              ifStmtElseBody =
                                Just
                                  [ IfStmt
                                      { ifStmtCondition = ExprNumber 3,
                                        ifStmtBody =
                                          [ ExprStmt
                                              { exprStmtBody = ExprNumber 4
                                              }
                                          ],
                                        ifStmtElseBody =
                                          Just
                                            [ExprStmt {exprStmtBody = ExprNumber 5}]
                                      }
                                  ]
                            }
                        ]
                    )
              ),
            testCase
              "with elif/else (advanced)"
              ( parseString
                  "if 1 do 2 . elif 3 do 4 . elif 5 do 6 . elif 7 do 8 . else 9 . if 10 do 11 . end end if 12 do 13 . elif 14 do 15 . else 16 . end"
                  @?= Right
                    ( Program
                        [ IfStmt
                            { ifStmtCondition = ExprNumber 1,
                              ifStmtBody = [ExprStmt {exprStmtBody = ExprNumber 2}],
                              ifStmtElseBody =
                                Just
                                  [ IfStmt
                                      { ifStmtCondition = ExprNumber 3,
                                        ifStmtBody =
                                          [ExprStmt {exprStmtBody = ExprNumber 4}],
                                        ifStmtElseBody =
                                          Just
                                            [ IfStmt
                                                { ifStmtCondition = ExprNumber 5,
                                                  ifStmtBody =
                                                    [ExprStmt {exprStmtBody = ExprNumber 6}],
                                                  ifStmtElseBody =
                                                    Just
                                                      [ IfStmt
                                                          { ifStmtCondition = ExprNumber 7,
                                                            ifStmtBody =
                                                              [ ExprStmt
                                                                  { exprStmtBody = ExprNumber 8
                                                                  }
                                                              ],
                                                            ifStmtElseBody =
                                                              Just
                                                                [ ExprStmt
                                                                    { exprStmtBody = ExprNumber 9
                                                                    },
                                                                  IfStmt
                                                                    { ifStmtCondition = ExprNumber 10,
                                                                      ifStmtBody =
                                                                        [ ExprStmt
                                                                            { exprStmtBody = ExprNumber 11
                                                                            }
                                                                        ],
                                                                      ifStmtElseBody = Nothing
                                                                    }
                                                                ]
                                                          }
                                                      ]
                                                }
                                            ]
                                      }
                                  ]
                            },
                          IfStmt
                            { ifStmtCondition = ExprNumber 12,
                              ifStmtBody = [ExprStmt {exprStmtBody = ExprNumber 13}],
                              ifStmtElseBody =
                                Just
                                  [ IfStmt
                                      { ifStmtCondition = ExprNumber 14,
                                        ifStmtBody =
                                          [ExprStmt {exprStmtBody = ExprNumber 15}],
                                        ifStmtElseBody =
                                          Just
                                            [ExprStmt {exprStmtBody = ExprNumber 16}]
                                      }
                                  ]
                            }
                        ]
                    )
              )
          ],
        testCase
          "while"
          ( parseString "while 1 do 2 . end"
              @?= Right
                (Program [WhileStmt (ExprNumber 1) [ExprStmt (ExprNumber 2)]])
          ),
        testCase
          "function"
          ( parseString "func test() do return 1 . end"
              @?= Right
                ( Program
                    [FunctionStmt "test" [] [ReturnStmt (Just (ExprNumber 1))]]
                )
          )
      ]
  ]

tests = testGroup "Parser" unitTests
