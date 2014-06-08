module TestParse
  (tests) where

import Capsir
import Capsir.Parse
import Test.HUnit

assertParses :: (Show a, Eq a) => (String -> Either e a) -> a -> String -> Assertion
assertParses parseFunc expected source = 
    case parseFunc source of
      Right testVal -> assertEqual "Didn't parse correctly" expected testVal
      Left _ -> fail $ "Input string did not parse: " ++ source

assertExprParses :: CpsExpr -> String -> Assertion
assertExprParses = assertParses parseCapsir

-- Actual Tests

testParseApply = TestLabel "Test Apply parses correctly" $ TestCase $
    assertExprParses
        (Apply [VarValue "a", VarValue "b"] $ VarValue "c")
        "(a, b) >> c"

testParseSingletonInst = TestLabel "Test Singleton Inst parses correctly" $ TestCase $
    assertExprParses
        (Inst "foo" [VarValue "a", VarValue "b"] [VarValue "c"])
        "foo(a, b) >> c"

testParseBranchInst = TestLabel "Test Branch Inst parses correctly" $ TestCase $
    assertExprParses
        (Inst "foo" [VarValue "a", VarValue "b"] [VarValue "c", VarValue "d"])
        "foo(a, b) ? { c | d }"

testParseExit = TestLabel "Test Exit parses correctly" $ TestCase $
    assertExprParses
        (Exit $ VarValue "a")
        "exit a"

-- Test Group

tests = TestLabel "Parse Tests" $ TestList
    [ testParseApply
    , testParseSingletonInst
    , testParseBranchInst
    , testParseExit
    ]
