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
  --testSucc fVariables0 [] $ FunctionProduct []
  ]

tests = test (atomTests ++ rexprTests)

main = runTestTT $ tests
