module Pristinum.ParserTest
  ( tests
  ) where

import           Pristinum.AST
import           Pristinum.Parser
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests =
  [ testGroup
    "expressions"
    [ testCase "null"  (parseString "null" @?= Right [ExprStmt LNull])
    , testCase "true"  (parseString "true" @?= Right [ExprStmt (LBool True)])
    , testCase "false" (parseString "false" @?= Right [ExprStmt (LBool False)])
    , testCase
      "string"
      (parseString "\"pristinum\"" @?= Right [ExprStmt (LString "pristinum")])
    , testCase "number"   (parseString "123" @?= Right [ExprStmt (LNumber 123)])
    , testCase "variable" (parseString "x" @?= Right [ExprStmt (Variable "x")])
    , testGroup
      "call"
      [ testCase "without arguments"
                 (parseString "x()" @?= Right [ExprStmt (Call "x" [])])
      , testCase
        "with arguments"
        (   parseString "x(1, 2, 3)"
        @?= Right [ExprStmt (Call "x" [LNumber 1, LNumber 2, LNumber 3])]
        )
      ]
    , testGroup
      "unary operation"
      [ testCase
        "negative"
        (parseString "-123" @?= Right [ExprStmt (Unary Negative (LNumber 123))])
      , testCase
        "bitwise not"
        (   parseString "~123"
        @?= Right [ExprStmt (Unary BitwiseNot (LNumber 123))]
        )
      , testCase
        "logical not"
        (   parseString "!true"
        @?= Right [ExprStmt (Unary LogicalNot (LBool True))]
        )
      ]
    , testGroup
      "binary operation"
      [ testCase
        "add"
        (   parseString "123 + 456"
        @?= Right [ExprStmt (Binary Add (LNumber 123) (LNumber 456))]
        )
      , testCase
        "subtract"
        (   parseString "123 - 456"
        @?= Right [ExprStmt (Binary Subtract (LNumber 123) (LNumber 456))]
        )
      , testCase
        "multiply"
        (   parseString "123 * 456"
        @?= Right [ExprStmt (Binary Multiply (LNumber 123) (LNumber 456))]
        )
      , testCase
        "divide"
        (   parseString "123 / 456"
        @?= Right [ExprStmt (Binary Divide (LNumber 123) (LNumber 456))]
        )
      , testCase
        "mod"
        (   parseString "123 % 456"
        @?= Right [ExprStmt (Binary Mod (LNumber 123) (LNumber 456))]
        )
      , testCase
        "bitwise shift left"
        (parseString "123 << 4" @?= Right
          [ExprStmt (Binary BitwiseShiftLeft (LNumber 123) (LNumber 4))]
        )
      , testCase
        "bitwise shift right"
        (parseString "123 >> 4" @?= Right
          [ExprStmt (Binary BitwiseShiftRight (LNumber 123) (LNumber 4))]
        )
      , testCase
        "bitwise and"
        (   parseString "123 & 456"
        @?= Right [ExprStmt (Binary BitwiseAnd (LNumber 123) (LNumber 456))]
        )
      , testCase
        "bitwise or"
        (   parseString "123 | 456"
        @?= Right [ExprStmt (Binary BitwiseOr (LNumber 123) (LNumber 456))]
        )
      , testCase
        "bitwise xor"
        (   parseString "123 ^ 456"
        @?= Right [ExprStmt (Binary BitwiseXor (LNumber 123) (LNumber 456))]
        )
      , testCase
        "equal"
        (   parseString "123 == 456"
        @?= Right [ExprStmt (Binary Equal (LNumber 123) (LNumber 456))]
        )
      , testCase
        "not equal"
        (   parseString "123 != 456"
        @?= Right [ExprStmt (Binary NotEqual (LNumber 123) (LNumber 456))]
        )
      , testCase
        "less"
        (   parseString "123 < 456"
        @?= Right [ExprStmt (Binary Less (LNumber 123) (LNumber 456))]
        )
      , testCase
        "greater"
        (   parseString "123 > 456"
        @?= Right [ExprStmt (Binary Greater (LNumber 123) (LNumber 456))]
        )
      , testCase
        "less equal"
        (   parseString "123 <= 456"
        @?= Right [ExprStmt (Binary LessEqual (LNumber 123) (LNumber 456))]
        )
      , testCase
        "greater equal"
        (parseString "123 >= 456" @?= Right
          [ExprStmt (Binary GreaterEqual (LNumber 123) (LNumber 456))]
        )
      , testCase
        "logical and"
        (   parseString "true && false"
        @?= Right [ExprStmt (Binary LogicalAnd (LBool True) (LBool False))]
        )
      , testCase
        "logical or"
        (   parseString "true || false"
        @?= Right [ExprStmt (Binary LogicalOr (LBool True) (LBool False))]
        )
      ]
    ]
  , testGroup
    "statements"
    [ testCase "expression"
               (parseString "true" @?= Right [ExprStmt (LBool True)])
    , testCase
      "let"
      (parseString "let x = true" @?= Right [LetStmt "x" (LBool True)])
    , testCase
      "assignment"
      (parseString "x = true" @?= Right [AssignStmt "x" (LBool True)])
    , testGroup
      "if"
      [ testCase
        "without else"
        (parseString "if 123 == 456: x = true end" @?= Right
          [ IfStmt (Binary Equal (LNumber 123) (LNumber 456))
                   [AssignStmt "x" (LBool True)]
                   Nothing
          ]
        )
      , testCase
        "with else"
        (parseString "if 123 == 456: x = true else x = false end" @?= Right
          [ IfStmt (Binary Equal (LNumber 123) (LNumber 456))
                   [AssignStmt "x" (LBool True)]
                   (Just [AssignStmt "x" (LBool False)])
          ]
        )
      ]
    , testCase
      "while"
      (parseString "while 123 == 456: x = true end" @?= Right
        [ WhileStmt (Binary Equal (LNumber 123) (LNumber 456))
                    [AssignStmt "x" (LBool True)]
        ]
      )
    , testCase
      "function"
      (parseString "@useless_sum (a, b): a + b end" @?= Right
        [ FunctionStmt "useless_sum"
                       ["a", "b"]
                       [ExprStmt (Binary Add (Variable "a") (Variable "b"))]
        ]
      )
    , testGroup
      "return"
      [ testCase "without expression"
                 (parseString "return" @?= Right [ReturnStmt Nothing])
      , testCase
        "with expression"
        (parseString "return 123 + 456" @?= Right
          [ReturnStmt (Just (Binary Add (LNumber 123) (LNumber 456)))]
        )
      ]
    ]
  ]

tests = testGroup "Parser" unitTests
