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
  FunctionProduct [FunctionVariable "a", FunctionVariable "b"],
  testSucc fRExpr (tok "-a") $
  FunctionProduct [FunctionNumber (negate 1.0), FunctionVariable "a"],
  testSucc fRExpr (tok "-3") $ FunctionNumber $ negate 3.0,
  testSucc fRExpr (tok "-2a b") $ FunctionProduct
  [FunctionNumber (negate 2), FunctionVariable "a", FunctionVariable "b"],
  testPart fRExpr (tok "2 3 a")
  (FunctionNumber 2.0) [Number 3.0, Variable "a"],
  testPart fRExpr (tok "-2 3 a")
  (FunctionNumber $ negate 2.0) [Number 3.0, Variable "a"]
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
  FunctionVariable "x",
  testFail fFExpr (tok "sin^-2 x"),
  testFail fFExpr (tok "sin^-x y"),
  testSucc fFExpr (tok "sin -1") $
  FunctionFunction "sin" $ FunctionNumber (-1.0),
  testSucc fFExpr (tok "sin -x") $
  FunctionFunction "sin" $ FunctionProduct [FunctionNumber (negate 1.0),
                                            FunctionVariable "x"],
  testSucc fFExpr (tok "sin cos tan x") $
  FunctionFunction "sin" $ FunctionFunction "cos" $ FunctionFunction "tan" $
  FunctionVariable "x",
  testPart fFExpr (tok "sin^x 2 y z 3")
  (FunctionPower (FunctionFunction "sin" $
                  FunctionProduct [FunctionNumber 2.0,
                                   FunctionVariable "y",
                                   FunctionVariable "z"])
   (FunctionVariable "x")) [Number 3.0],
  testPart fFExpr (tok "sin^2 3 4")
  (FunctionPower (FunctionFunction "sin" $ FunctionNumber 3.0)
   (FunctionNumber 2.0)) [Number 4.0],
  testPart fFExpr (tok "sin cos x 2")
  (FunctionFunction "sin" (FunctionFunction "cos" $ FunctionVariable "x"))
  [Number 2.0],
  testPart fFExpr (tok "sin 2 cos x")
  (FunctionFunction "sin" (FunctionNumber 2.0)) [Function "cos", Variable "x"]
  ]

juxtaposeAndExponentialTests = [
  testSucc fJExpr (tok "2") $ FunctionNumber 2.0,
  testSucc fJExpr (tok "-2") $ FunctionNumber (negate 2.0),
  testSucc fJExpr (tok "x") $ FunctionVariable "x",
  testSucc fJExpr (tok "-x") $ FunctionProduct [FunctionNumber $ negate 1.0,
                                                FunctionVariable "x"],
  testSucc fJExpr (tok "2x") $ FunctionProduct [FunctionNumber 2.0,
                                                FunctionVariable "x"],
  testSucc fJExpr (tok "-2x") $ FunctionProduct [FunctionNumber $ negate 2.0,
                                                 FunctionVariable "x"],
  testSucc fJExpr (tok "x y") $ FunctionProduct [FunctionVariable "x",
                                                 FunctionVariable "y"],
  testSucc fJExpr (tok "-x y") $ FunctionProduct [FunctionNumber $ negate 1.0,
                                                  FunctionVariable "x",
                                                  FunctionVariable "y"],
  testSucc fJExpr (tok "2x y") $ FunctionProduct [FunctionNumber 2.0,
                                                  FunctionVariable "x",
                                                  FunctionVariable "y"],
  testSucc fJExpr (tok "-2x y") $ FunctionProduct [FunctionNumber $ negate 2.0,
                                                   FunctionVariable "x",
                                                   FunctionVariable "y"]
  -- The below test passes.  Is this bad?
  -- testSucc fJExpr (tok "2 3") (FunctionNumber 6.0)
  ]

tests = test (atomTests ++ rexprTests ++ functionTests ++
              juxtaposeAndExponentialTests)

main = runTestTT $ tests
