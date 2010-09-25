-- pToken only really needs to be exported for testing purposes
module Tokenizer (pTokenizer, pToken, Token(..)) where

import Parser
import Data.Char
import Control.Monad

data Token = Number Double | Function String | Variable String | Symbol Char
           deriving (Eq, Show)

isSymbol :: Token -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

pTokenizer :: Parser Char [Token]
pTokenizer = do ts <- pStar (pSpaces >> pToken)
                pSpaces
                return ts

pSpaces :: Parser Char ()
pSpaces = do pStar (pProp isSpace)
             return ()

pToken :: Parser Char Token
pToken = pNumber ||| pFunction ||| pVariable ||| pSymbol

pNumber :: Parser Char Token
pNumber = pNumber1 ||| pNumber2

pNumber1 = do whole <- pPlus (pProp isDigit)
              frac <- pMaybe $ do pElt '.'
                                  pStar (pProp isDigit)
              return . Number $ read (case frac of
                                         Just digits ->
                                           whole ++ "." ++ digits ++ "0"
                                         Nothing -> whole)

pNumber2 = do pElt '.'
              frac <- pPlus (pProp isDigit)
              return . Number $ read ("0." ++ frac)

pFunction :: Parser Char Token
pFunction = foldr1 mplus (map makeParseWord functions)
  where
    makeParseWord w = do pWord w
                         pNot (pProp isAlphaNum)
                         return $ Function w

functions :: [String]
functions = trigFunctions ++ inverseTrigFunctions ++
            logFunctions ++ expFunctions

trigFunctions = ["sin", "cos", "tan", "cot", "sec", "csc"]

inverseTrigFunctions = map ('a':) trigFunctions

logFunctions = ["log", "ln"]

expFunctions = ["exp"]

pVariable :: Parser Char Token
pVariable = do x <- pProp isAlpha
               xs <- pStar (pProp isAlphaNum)
               return $ Variable (x:xs)

pSymbol :: Parser Char Token
pSymbol = foldr1 mplus (map makeParseSymbol symbols)
  where
    makeParseSymbol :: Char -> Parser Char Token
    makeParseSymbol c = pElt c >> return (Symbol c)

symbols = "()+-*/^"
