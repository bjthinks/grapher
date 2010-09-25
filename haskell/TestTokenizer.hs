import Test.HUnit
import Parser
import Tokenizer
import Control.Monad

isFail :: Either a b -> Bool
isFail (Left _) = True
isFail (Right _) = False

testPart :: (Show a, Eq a) => Parser Char a -> String -> a -> String -> Test
testPart parser input output rest
  = test $ parseSome parser input ~?= Right
    (output, length input - length rest, rest)

testSucc :: (Show a, Eq a) => Parser Char a -> String -> a -> Test
testSucc parser input output
  = testPart parser input output ""

testFail :: (Show t) => Parser t a -> [t] -> Test
testFail parser input
  = test $ unless (isFail $ parseSome parser input)
    (assertFailure "Expected failure, but succeeded")

numberTests = [
  testFail pToken "",
  testFail pToken ".",
  testSucc pToken "0." $ Number 0.0,
  testSucc pToken "0." $ Number 0.0,
  testSucc pToken "3." $ Number 3.0,
  testSucc pToken ".5" $ Number 0.5,
  testSucc pToken "123.25" $ Number 123.25,
  testPart pToken "1.25.125" (Number 1.25) ".125",
  testPart pToken ".5.5" (Number 0.5) ".5",
  testSucc pToken "3" $ Number 3
  ]

functions = ["sin", "cos", "tan", "sec", "cot", "csc",
             "asin", "acos", "atan", "asec", "acot", "acsc",
             "log", "ln", "exp"]

functionTests = do func <- functions
                   return $ func ~: test [
                     foo func Function,
                     foo (func ++ "x") Variable ]
  where foo spam eggs =
          testSucc pToken spam $ eggs spam

variableTests = [
  testSucc pToken "a" $ Variable "a",
  testSucc pToken "a3b45c6" $ Variable "a3b45c6"
  ]

symbols = "+-*/()^"
notSymbols = "!@#$%&_={}[]|:;<>,.?"
symbolTests = map (\c -> testSucc pToken [c] $ Symbol c) symbols ++
              map (\c -> testFail pToken [c]) notSymbols

tokenizerTests = [
  testSucc pTokenizer "" [],
  testSucc pTokenizer "                      " [],
  testSucc pTokenizer "   3   4   5   " [Number 3.0, Number 4.0, Number 5.0],
  testSucc pTokenizer "((" [Symbol '(', Symbol '(']
  ]

tests = test (numberTests ++ functionTests ++ variableTests ++ symbolTests ++
              tokenizerTests)

main = runTestTT $ tests
