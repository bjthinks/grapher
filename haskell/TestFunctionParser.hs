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

functionTests = [
  testSucc fFExpr [Function "sin", Variable "x"] $
  FunctionFunction "sin" (FunctionVariable "x"),
  testFail fFExpr [Function "sin"],
  testFail fFExpr [Variable "x"],
  testSucc fFExpr [Function "sin", Symbol '^', Symbol '-', Number 1.0,
                   Variable "x"] $
  FunctionInverse $ FunctionFunction "sin" $ FunctionVariable "x",
  testSucc fFExpr [Function "sin", Symbol '^', Number 2.0, Variable "x"] $
  FunctionPower (FunctionFunction "sin" $ FunctionVariable "x") $
  FunctionNumber 2.0,
  testSucc fFExpr [Function "sin", Symbol '^', Variable "x", Variable "y"] $
  FunctionPower (FunctionFunction "sin" $ FunctionVariable "y") $
  FunctionVariable "x",
  testSucc fFExpr [Function "sin", Function "cos", Function "tan",
                   Variable "x"] $
  FunctionFunction "sin" $ FunctionFunction "cos" $ FunctionFunction "tan" $
  FunctionVariable "x",
  testSucc fFExpr [Function "sin", Symbol '-', Number 1.0] $
  FunctionFunction "sin" $ FunctionNumber (-1.0)
  ]

tests = test (atomTests ++ rexprTests ++ functionTests)

main = runTestTT $ tests
