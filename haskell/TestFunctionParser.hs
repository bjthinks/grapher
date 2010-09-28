import Test.HUnit
import Parser
import Tokenizer
import FunctionParser
import Control.Monad

isFail :: Either a b -> Bool
isFail (Left _) = True
isFail (Right _) = False

testPart :: (Show t, Eq t, Show a, Eq a) =>
            Parser t a -> [t] -> a -> [t] -> Test
testPart parser input output rest
  = test $ parseSome parser input ~?= Right
    (output, length input - length rest, rest)

testSucc :: (Show t, Eq t, Show a, Eq a) => Parser t a -> [t] -> a -> Test
testSucc parser input output
  = testPart parser input output []

testFail :: (Show t) => Parser t a -> [t] -> Test
testFail parser input
  = test $ unless (isFail $ parseSome parser input)
    (assertFailure "Expected failure, but succeeded")

tok :: String -> [Token]
tok str = case parseAll pTokenizer str of
  Left _ -> error "Could not tokenize test string"
  Right ts -> ts

atomTests = [
  testSucc fNumber (tok "3") $ FunctionNumber 3.0,
  testFail fNumber (tok ""),
  testFail fNumber (tok "abc"),
  testFail fNumber (tok "sin"),
  testFail fNumber (tok "+"),
  testSucc fVariable (tok "x") $ FunctionVariable "x",
  testFail fVariable (tok ""),
  testFail fVariable (tok "1.5"),
  testFail fVariable (tok "cos"),
  testFail fVariable (tok "(")
  ]

rexprTests = [
  testSucc fVariables0 (tok "") $ FunctionProduct [],
  testSucc fVariables0 (tok "a") $ FunctionVariable "a",
  testSucc fVariables0 (tok "a b c") $
  FunctionProduct [FunctionVariable "a", FunctionVariable "b",
                   FunctionVariable "c"],
  testPart fVariables0 (tok "a b 1")
  (FunctionProduct [FunctionVariable "a", FunctionVariable "b"]) [Number 1.0],
  testFail fVariables1 (tok ""),
  testSucc fVariables1 (tok "a") $ FunctionVariable "a",
  testSucc fVariables1 (tok "a b c") $
  FunctionProduct [FunctionVariable "a", FunctionVariable "b",
                   FunctionVariable "c"],
  testPart fVariables1 (tok "a b 1")
  (FunctionProduct [FunctionVariable "a", FunctionVariable "b"]) [Number 1.0],
  testFail fRExpr (tok ""),
  testSucc fRExpr (tok "2") $ FunctionNumber 2.0,
  testSucc fRExpr (tok "a") $ FunctionVariable "a",
  testPart fRExpr (tok "1 2") (FunctionNumber 1.0) [Number 2.0],
  testSucc fRExpr (tok "1 a") $
  FunctionProduct [FunctionNumber 1.0, FunctionVariable "a"],
  testSucc fRExpr (tok "a b") $
  FunctionProduct [FunctionVariable "a", FunctionVariable "b"]
  ]

functionTests = [
  testSucc fFExpr (tok "sin x") $
  FunctionFunction "sin" (FunctionVariable "x"),
  testFail fFExpr (tok "sin"),
  testFail fFExpr (tok "x"),
  testSucc fFExpr (tok "sin^-1 x") $
  FunctionInverse $ FunctionFunction "sin" $ FunctionVariable "x",
  testSucc fFExpr (tok "sin^2 x") $
  FunctionPower (FunctionFunction "sin" $ FunctionVariable "x") $
  FunctionNumber 2.0,
  testSucc fFExpr (tok "sin^x y") $
  FunctionPower (FunctionFunction "sin" $ FunctionVariable "y") $
  FunctionVariable "x"
  ]

tests = test (atomTests ++ rexprTests ++ functionTests)

main = runTestTT $ tests
