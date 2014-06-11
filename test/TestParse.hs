module TestParse
  (tests) where

import Capsir
import Capsir.Parse
import Text.ParserCombinators.Parsec.Error
import Test.HUnit
import qualified Data.String.Utils as Utils

assertParses :: (Show a, Eq a) => (String -> Either ParseError a) -> a -> String -> Assertion
assertParses parseFunc expected source = 
    case parseFunc source of
      Right testVal -> assertEqual "Didn't parse correctly" expected testVal
      Left e ->
          let msgs = errorMessages e
              errorStrings = map messageString msgs
          in fail ("Input string did not parse: " ++ source ++ "\n" ++ (Utils.join "\n" errorStrings))

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

-- Test value parsing

testParseLiteralInt = TestLabel "Test A Literal Parses Correctly" $ TestCase $
    assertExprParses
        (Exit $ LitValue $ Literal "int" [LitParamInt 5])
        "exit @int(5)"

-- Test Group

tests = TestLabel "Parse Tests" $ TestList
    [ testParseApply
    , testParseSingletonInst
    , testParseBranchInst
    , testParseExit
    , testParseLiteralInt
    ]
