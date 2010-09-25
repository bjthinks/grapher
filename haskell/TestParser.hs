import Test.HUnit
import Monad
import Parser
import Char

isFail :: Either a b -> Bool
isFail (Left _) = True
isFail (Right _) = False

-- Check that Parser is exported
pDigit :: Parser Char Char
pDigit = pProp isDigit

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

testEquiv :: (Eq a, Eq t) => Parser t a -> Parser t a -> [[t]] -> Test
testEquiv parser1 parser2 inputs
  = TestCase $ unless
    (map (parseSome parser1) inputs == map (parseSome parser2) inputs)
    (assertFailure "Inequivalent parsers")

testError :: (Eq a, Eq t) => Parser t a -> [t] -> Int -> [(Int, String)] ->
             Test
testError parser input error_position names
  = test $ unless
    ((actual_position, actual_names) == (error_position, names))
    (assertFailure $ "Wrong error, expected "
     ++ show (error_position, names) ++ " but got "
     ++ show (actual_position, actual_names))
      where
        Left error_result = parseSome parser input
        actual_position = errorLocation error_result
        actual_names = errorNames error_result


basicTests = [
  -- parseSome and pGet
  "Parse any ''"    ~: testFail pGet "",
  "Parse any a"     ~: testSucc pGet "a" 'a',
  "Parse any ab"    ~: testPart pGet "ab" 'a' "b",
  "Parse any abcde" ~: testPart pGet "abcde" 'a' "bcde",
  -- Monad: bind (>>=), return, fail
  -- Monad laws
  "Monad left identity" ~:
  testEquiv (do x <- return 'x' ; pElt x) (pElt 'x')
  ["", "x", "y", "xy", "yx", "xx", "yy", "xyz"],
  "Monad right identity" ~:
  testEquiv (do x <- pElt 'x' ; return x) (pElt 'x')
  ["", "x", "y", "xy", "yx", "xx", "yy", "xyz"],
  "Monad associativity" ~:
  testEquiv (do y <- (do x <- pGet
                         pElt x)
                pElt (toUpper y))
            (do x <- pGet
                (do y <- pElt x
                    pElt (toUpper y)))
            [x:y:z:[] | x <- "abAB", y <- "abAB", z <- "abAB"],
  -- A few other tests of monadic functionality
  "Parse return 123" ~: testSucc (return 123) "" 123,
  "Parse return word 123 '123'" ~: testSucc (do s <- pWord "123"
                                                return s) "123" "123",
  "Parse any2 ''"  ~: testFail (pGet >> pGet) "",
  "Parse any2 a"   ~: testFail (pGet >> pGet) "a",
  "Parse any2 ab"  ~: testSucc (do x <- pGet
                                   y <- pGet
                                   return (x,y)) "ab" ('a','b'),
  "Parse any2 abc" ~: testPart (do x <- pGet
                                   y <- pGet
                                   return (x,y)) "abc" ('a','b') "c",
  "Parse fail ''"  ~: testFail (fail "") "",
  "Parse fail abc" ~: testFail (fail "") "abc",
  -- MonadPlus: mzero and |||
  -- MonadPlus laws
  "MonadPlus left id 1" ~:
  testEquiv (mplus mzero pGet) pGet ["", "x", "y", "xy"],
  "MonadPlus left id 2" ~:
  testEquiv (mplus mzero (pElt 'x')) (pElt 'x') ["", "x", "y", "xy"],
  "MonadPlus right id 1" ~:
  testEquiv (mplus pGet mzero) pGet ["", "x", "y", "xy"],
  "MonadPlus right id 2" ~:
  testEquiv (mplus (pElt 'x') mzero) (pElt 'x') ["", "x", "y", "xy"],
  let
    a = "error a" $= pElt 'a'
    b = "error b" $= pElt 'b'
    c = "error c" $= pElt 'c'
    inputs = [""] ++ [[x] | x <- "abcd"] ++
             [[x,y] | x <- "abcd", y <- "abcd"] ++
             [[x,y,z] | x <- "abcd", y <- "abcd", z <- "abcd"]
  in "MonadPlus associativity" ~:
     testEquiv (mplus (mplus a b) c) (mplus a (mplus b c)) inputs,
  "MonadPlus left zero" ~: testEquiv (mzero >>= pElt) mzero ["", "x", "xy"],
  "MonadPlus left catch" ~:
  testEquiv (mplus (return 'x') (pElt 'y')) (return 'x')
  ["", "x", "y", "xx", "xy", "yx", "yy"],
  -- More basic tests of MonadPlus functionality
  "Parse mzero ''"  ~: testFail mzero "",
  "Parse mzero a"   ~: testFail mzero "a",
  "Parse mzero ab"  ~: testFail mzero "ab",
  "Parse mzero abc" ~: testFail mzero "abc",
  "Parse a or b a" ~: testSucc (mplus (pElt 'a') (pElt 'b')) "a" 'a',
  "Parse a or b b" ~: testSucc (mplus (pElt 'a') (pElt 'b')) "b" 'b',
  "Parse a or b c" ~: testFail (mplus (pElt 'a') (pElt 'b')) "c",
  -- pIf
  "ParseSome (pIf pGet) ''"   ~: testFail (pIf pGet) "",
  "ParseSome (pIf pGet) 'a'"  ~: testPart (pIf pGet) "a" 'a' "a",
  "ParseSome (pIf pGet) 'ab'" ~: testPart (pIf pGet) "ab" 'a' "ab",
  "Equal Ifget Ififget" ~:
  testEquiv (pIf pGet) (pIf $ pIf pGet) ["", "a", "ab"],
  "Ifget ifget ''" ~: testFail (do x <- pIf pGet
                                   y <- pIf pGet
                                   return (x,y)) "",
  "Ifget ifget a"  ~: testPart (do x <- pIf pGet
                                   y <- pIf pGet
                                   return (x,y)) "a" ('a','a') "a",
  "Ifget ifget ab" ~: testPart (do x <- pIf pGet
                                   y <- pIf pGet
                                   return (x,y)) "ab" ('a','a') "ab",
  -- pNot
  "Parse (pNot pGet) ''" ~: testSucc (pNot pGet) "" (),
  "Parse (pNot pGet) a"  ~: testFail (pNot pGet) "a",
  "Parse (pNot pGet) aa" ~: testFail (pNot pGet) "aa",
  "Parse (pNot pNot pGet) ''" ~: testFail (pNot $ pNot pGet) "",
  "Parse (pNot pNot pGet) a"  ~: testPart (pNot $ pNot pGet) "a" () "a",
  "Parse (pNot pNot pGet) ab" ~: testPart (pNot $ pNot pGet) "ab" () "ab",
  "Equal notget, notget notget" ~:
  testEquiv (pNot pGet) (pNot pGet >> pNot pGet) ["", "a", "ab"]
  ]

extraTests = [
  -- parseAll
  "Parse any aa (with parseAll)" ~: testPart pGet "aa" 'a' "a",
  -- pEnd
  "Parse end ''" ~: parseSome pEnd "" ~?= Right ((), 0, ""),
  "Parse end a" ~: isFail (parseSome pEnd "a") ~?= True,
  -- pProp
  "Parse digit 3"  ~: testSucc (pProp isDigit) "3" '3',
  "Parse digit 0"  ~: testSucc (pProp isDigit) "0" '0',
  "Parse digit %"  ~: testFail (pProp isDigit) "%",
  "Parse digit ''" ~: testFail (pProp isDigit) "",
  "Parse digit 66" ~: testPart (pProp isDigit) "66" '6' "6",
  "Parse digit lower 6h" ~: testSucc (do d <- pProp isDigit
                                         r <- pProp isLower
                                         return (d, r)) "6h" ('6', 'h'),
  "Parse digit lower 6H" ~: testFail (do d <- pProp isDigit
                                         r <- pProp isLower
                                         return (d, r)) "6H",
  "Parse digit lower gh" ~: testFail (do d <- pProp isDigit
                                         r <- pProp isLower
                                         return (d, r)) "gh",
  -- pElt
  "Parse character x x"  ~: testSucc (pElt 'x') "x" 'x',
  "Parse character x y"  ~: testFail (pElt 'x') "y",
  "Parse character x xy" ~: testPart (pElt 'x') "xy" 'x' "y",
  "Parse character x ''" ~: testFail (pElt 'x') "",
  -- pWord
  "Parse word abc abc"  ~: testSucc (pWord "abc") "abc" "abc",
  "Parse word abc abd"  ~: testFail (pWord "abc") "abd",
  "Parse word abc abcd" ~: testPart (pWord "abc") "abcd" "abc" "d",
  "Parse word abc ab"   ~: testFail (pWord "abc") "ab",
  -- pStar
  "Parse pGet* ''" ~: parseAll (pStar pGet) "" ~?= Right "",
  "Parse pGet* 'a'" ~: parseAll (pStar pGet) "a" ~?= Right "a",
  "Parse pGet* 'abcde'" ~: parseAll (pStar pGet) "abcde" ~?= Right "abcde",
  "Parse pLower* 'abcde'" ~:
  testSucc (pStar (pProp isLower)) "abcde" "abcde",
  "Parse pLower* 'abcdE'" ~:
  testPart (pStar (pProp isLower)) "abcdE" "abcd" "E",
  "Parse pLower* pUpper 'abcdE'" ~:
  testSucc (do xs <- pStar (pProp isLower)
               y <- pProp isUpper
               return (xs,y)) "abcdE" ("abcd",'E'),
  "Parse pLower* pLower 'abcde'" ~:
  testFail (pStar (pProp isLower) >> pProp isLower) "abcde",
  -- pPlus
  "Parse pGet+ ''" ~: testFail (pPlus pGet) "",
  "Parse pGet+ 'a'" ~: parseAll (pPlus pGet) "a" ~?= Right "a",
  "Parse pGet+ 'abcde'" ~: parseAll (pPlus pGet) "abcde" ~?= Right "abcde",
  "Parse pLower+ 'abcde'" ~:
  testSucc (pPlus (pProp isLower)) "abcde" "abcde",
  "Parse pLower+ 'abcdE'" ~:
  testPart (pPlus (pProp isLower)) "abcdE" "abcd" "E",
  "Parse pLower+ pUpper 'abcdE'" ~:
  testSucc (do xs <- pPlus (pProp isLower)
               y <- pProp isUpper
               return (xs,y)) "abcdE" ("abcd",'E'),
  "Parse pLower+ pLower 'abcde'" ~:
  testFail (pPlus (pProp isLower) >> pProp isLower) "abcde",
  -- pMaybe
  "Parse pMaybe pGet ''" ~: parseSome (pMaybe pGet) "" ~?=
  Right (Nothing, 0, ""),
  "Parse pMaybe pGet 'a'" ~: testSucc (pMaybe pGet) "a" (Just 'a'),
  "Parse pMaybe pGet 'aa'" ~: testPart (pMaybe pGet) "aa" (Just 'a') "a",
  "Parse (pMaybe pLower) (pMaybe pUpper) ''" ~:
  testSucc (do x <- pMaybe (pProp isLower)
               y <- pMaybe (pProp isUpper)
               return (x,y)) "" (Nothing,Nothing),
  "Parse (pMaybe pLower) (pMaybe pUpper) 'a'" ~:
  testSucc (do x <- pMaybe (pProp isLower)
               y <- pMaybe (pProp isUpper)
               return (x,y)) "a" (Just 'a',Nothing),
  "Parse (pMaybe pLower) (pMaybe pUpper) 'A'" ~:
  testSucc (do x <- pMaybe (pProp isLower)
               y <- pMaybe (pProp isUpper)
               return (x,y)) "A" (Nothing,Just 'A'),
  "Parse (pMaybe pLower) (pMaybe pUpper) 'aA'" ~:
  testSucc (do x <- pMaybe (pProp isLower)
               y <- pMaybe (pProp isUpper)
               return (x,y)) "aA" (Just 'a',Just 'A'),
  "Parse (pMaybe pLower) (pMaybe pUpper) 'aa'" ~:
  testPart (do x <- pMaybe (pProp isLower)
               y <- pMaybe (pProp isUpper)
               return (x,y)) "aa" (Just 'a',Nothing) "a",
  "Parse (pMaybe pLower) (pMaybe pUpper) 'AA'" ~:
  testPart (do x <- pMaybe (pProp isLower)
               y <- pMaybe (pProp isUpper)
               return (x,y)) "AA" (Nothing,Just 'A') "A",
  "Parse (pMaybe pLower) (pMaybe pUpper) 'Aa'" ~:
  testPart (do x <- pMaybe (pProp isLower)
               y <- pMaybe (pProp isUpper)
               return (x,y)) "Aa" (Nothing,Just 'A') "a",
  "Parse (pMaybe pLower) (pMaybe pUpper) 'aaa'" ~:
  testPart (do x <- pMaybe (pProp isLower)
               y <- pMaybe (pProp isUpper)
               return (x,y)) "aaa" (Just 'a',Nothing) "aa",
  "Parse (pMaybe pLower) (pMaybe pUpper) 'aaA'" ~:
  testPart (do x <- pMaybe (pProp isLower)
               y <- pMaybe (pProp isUpper)
               return (x,y)) "aaA" (Just 'a',Nothing) "aA",
  "Parse (pMaybe pLower) (pMaybe pUpper) 'aAA'" ~:
  testPart (do x <- pMaybe (pProp isLower)
               y <- pMaybe (pProp isUpper)
               return (x,y)) "aAA" (Just 'a',Just 'A') "A",
  "Parse (pMaybe pLower) (pMaybe pUpper) 'AAA'" ~:
  testPart (do x <- pMaybe (pProp isLower)
               y <- pMaybe (pProp isUpper)
               return (x,y)) "AAA" (Nothing,Just 'A') "AA",
  -- pIf + pProp
  "Parse lower case hex digit 'a'" ~: testSucc (do pIf (pProp isHexDigit)
                                                   pProp isLower) "a" 'a',
  "Parse lower case hex digit 'f'" ~: testSucc (do pIf (pProp isHexDigit)
                                                   pProp isLower) "f" 'f',
  "Parse lower case hex digit 'g'" ~: testFail (do pIf (pProp isHexDigit)
                                                   pProp isLower) "g",
  "Parse lower case hex digit 'A'" ~: testFail (do pIf (pProp isHexDigit)
                                                   pProp isLower) "A",
  "Parse lower case hex digit '1'" ~: testFail (do pIf (pProp isHexDigit)
                                                   pProp isLower) "1",
  -- pIf + pNot + pProp
  "Parse hex digit not lower '1'" ~: testSucc (do pIf (pProp isHexDigit)
                                                  pNot (pProp isLower)
                                                  pGet) "1" '1',
  "Parse hex digit not lower 'd'" ~: testFail (do pIf (pProp isHexDigit)
                                                  pNot (pProp isLower)
                                                  pGet) "f",
  "Parse hex digit not lower 'g'" ~: testFail (do pIf (pProp isHexDigit)
                                                  pNot (pProp isLower)
                                                  pGet) "g",
  "Parse hex digit not lower 'A'" ~: testSucc (do x <- pIf (pProp isHexDigit)
                                                  y <- pNot (pProp isLower)
                                                  z <- pGet
                                                  return (x,y,z))
                                      "A" ('A',(),'A'),
  "Parse hex digit not lower 'G'" ~: testFail (do pIf (pProp isHexDigit)
                                                  pNot (pProp isLower)
                                                  pGet) "G"
  ]

errorTests = [
  -- Naming pGet
  "g ''"       ~: testError                      pGet  "" 0 [],
  "{s}g ''"    ~: testError (          "spam" $= pGet) "" 0 [(0,"spam")],
  "{e}{s}g ''" ~: testError ("eggs" $= "spam" $= pGet) "" 0 [(0,"spam"),(0,"eggs")],

  -- Naming pEnd
  "e a"       ~: testError                      pEnd  "a" 0 [],
  "{s}e a"    ~: testError (          "spam" $= pEnd) "a" 0 [(0,"spam")],
  "{e}{s}e a" ~: testError ("eggs" $= "spam" $= pEnd) "a" 0 [(0,"spam"),(0,"eggs")],

  -- Naming pGet >> pGet
  "gg ''"            ~: testError                 (          pGet >>           pGet)  ""  0 [],
  "gg a"             ~: testError                 (          pGet >>           pGet)  "a" 1 [],
  "g{e}g ''"         ~: testError                 (          pGet >> "eggs" $= pGet)  ""  0 [],
  "g{e}g a"          ~: testError                 (          pGet >> "eggs" $= pGet)  "a" 1 [(1,"eggs")],
  "{s}gg ''"         ~: testError                 ("spam" $= pGet >>           pGet)  ""  0 [(0,"spam")],
  "{s}gg a"          ~: testError                 ("spam" $= pGet >>           pGet)  "a" 1 [],
  "{s}g{e}g ''"      ~: testError                 ("spam" $= pGet >> "eggs" $= pGet)  ""  0 [(0,"spam")],
  "{s}g{e}g a"       ~: testError                 ("spam" $= pGet >> "eggs" $= pGet)  "a" 1 [(1,"eggs")],
  "{h}(gg) ''"       ~: testError ("shrubbery" $= (          pGet >>           pGet)) ""  0 [           (0,"shrubbery")],
  "{h}(gg) a"        ~: testError ("shrubbery" $= (          pGet >>           pGet)) "a" 1 [           (0,"shrubbery")],
  "{h}(g{e}g) ''"    ~: testError ("shrubbery" $= (          pGet >> "eggs" $= pGet)) ""  0 [           (0,"shrubbery")],
  "{h}(g{e}g) a"     ~: testError ("shrubbery" $= (          pGet >> "eggs" $= pGet)) "a" 1 [(1,"eggs"),(0,"shrubbery")],
  "{h}({s}gg) ''"    ~: testError ("shrubbery" $= ("spam" $= pGet >>           pGet)) ""  0 [(0,"spam"),(0,"shrubbery")],
  "{h}({s}gg) a"     ~: testError ("shrubbery" $= ("spam" $= pGet >>           pGet)) "a" 1 [           (0,"shrubbery")],
  "{h}({s}g{e}g) ''" ~: testError ("shrubbery" $= ("spam" $= pGet >> "eggs" $= pGet)) ""  0 [(0,"spam"),(0,"shrubbery")],
  "{h}({s}g{e}g) a"  ~: testError ("shrubbery" $= ("spam" $= pGet >> "eggs" $= pGet)) "a" 1 [(1,"eggs"),(0,"shrubbery")],

  -- Naming pGet ||| mzero
  "{s}g|z ''"         ~: testError                 (("spam" $= pGet) |||            mzero )  "" 0 [],
  "g|{e}z ''"         ~: testError                 (           pGet  ||| ("eggs" $= mzero))  "" 0 [(0,"eggs")],
  "{h}(g|z) ''"       ~: testError ("shrubbery" $= (           pGet  |||            mzero )) "" 0 [(0,"shrubbery")],
  "{h}({s}g|{e}z) ''" ~: testError ("shrubbery" $= (("spam" $= pGet) ||| ("eggs" $= mzero))) "" 0 [(0,"eggs"),(0,"shrubbery")],

  -- Naming pElt 'a' ||| pElt 'b'
  "{h}({s}a|{e}b) ''" ~: testError (pNamed "shrubbery" (pNamed "spam" (pElt 'a') ||| pNamed "eggs" (pElt 'b'))) ""  0 [(0,"eggs"),(0,"shrubbery")],
  "{h}({s}a|{e}b) c"  ~: testError (pNamed "shrubbery" (pNamed "spam" (pElt 'a') ||| pNamed "eggs" (pElt 'b'))) "c" 0 [(0,"eggs"),(0,"shrubbery")],

  -- Naming pElt 'a' >> pElt 'b'
  "{h}({s}a>{e}b) ''" ~: testError (pNamed "shrubbery" (pNamed "spam" (pElt 'a') >> pNamed "eggs" (pElt 'b'))) ""   0 [(0,"spam"),(0,"shrubbery")],
  "{h}({s}a>{e}b) b"  ~: testError (pNamed "shrubbery" (pNamed "spam" (pElt 'a') >> pNamed "eggs" (pElt 'b'))) "b"  0 [(0,"spam"),(0,"shrubbery")],
  "{h}({s}a>{e}b) a"  ~: testError (pNamed "shrubbery" (pNamed "spam" (pElt 'a') >> pNamed "eggs" (pElt 'b'))) "a"  1 [(1,"eggs"),(0,"shrubbery")],
  "{h}({s}a>{e}b) aa" ~: testError (pNamed "shrubbery" (pNamed "spam" (pElt 'a') >> pNamed "eggs" (pElt 'b'))) "aa" 1 [(1,"eggs"),(0,"shrubbery")]
  ]

tests = test (basicTests ++ extraTests ++ errorTests)

main = runTestTT tests
