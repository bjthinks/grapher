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

atomTests = [
  testSucc fNumber [Number 3.0] $ FunctionNumber 3.0,
  testFail fNumber [],
  testFail fNumber [Variable "abc"],
  testFail fNumber [Function "sin"],
  testFail fNumber [Symbol '+'],
  testSucc fVariable [Variable "x"] $ FunctionVariable "x",
  testFail fVariable [],
  testFail fVariable [Number 1.5],
  testFail fVariable [Function "cos"],
  testFail fVariable [Symbol '(']
  ]

rexprTests = [
  testSucc fVariables0 [] $ FunctionProduct [],
  testSucc fVariables0 [Variable "a"] $ FunctionVariable "a",
  testSucc fVariables0 [Variable "a", Variable "b", Variable "c"] $
  FunctionProduct [FunctionVariable "a", FunctionVariable "b",
                   FunctionVariable "c"],
  testPart fVariables0 [Variable "a", Variable "b", Number 1.0]
  (FunctionProduct [FunctionVariable "a", FunctionVariable "b"]) [Number 1.0],
  testFail fVariables1 [],
  testSucc fVariables1 [Variable "a"] $ FunctionVariable "a",
  testSucc fVariables1 [Variable "a", Variable "b", Variable "c"] $
  FunctionProduct [FunctionVariable "a", FunctionVariable "b",
                   FunctionVariable "c"],
  testPart fVariables1 [Variable "a", Variable "b", Number 1.0]
  (FunctionProduct [FunctionVariable "a", FunctionVariable "b"]) [Number 1.0],
  testFail fRExpr [],
  testSucc fRExpr [Number 2.0] $ FunctionNumber 2.0,
  testSucc fRExpr [Variable "a"] $ FunctionVariable "a",
  testPart fRExpr [Number 1.0, Number 2.0] (FunctionNumber 1.0) [Number 2.0],
  testSucc fRExpr [Number 1.0, Variable "a"] $
  FunctionProduct [FunctionNumber 1.0, FunctionVariable "a"],
  testSucc fRExpr [Variable "a", Variable "b"] $
  FunctionProduct [FunctionVariable "a", FunctionVariable "b"]
  ]

tok :: String -> [Token]
tok str = case parseAll pTokenizer str of
  Left _ -> error "Could not tokenize test string"
  Right ts -> ts

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
