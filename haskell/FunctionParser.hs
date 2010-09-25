module FunctionParser where

import Parser
import Tokenizer
--import Control.Monad

data Function = FunctionSum [Function] |
                FunctionProduct [Function] |
                FunctionDifference Function Function |
                FunctionQuotient Function Function |
                FunctionPower Function Function |
                FunctionFunction String Function |
                FunctionVariable String |
                FunctionNumber Double
                deriving (Show, Eq)

{-
aexpr = mexpr (('+' | '-') mexpr)*
mexpr = jexpr (('*' | '/') jexpr)*
jexpr = '-'? eexpr+
eexpr = atom ('^' jexpr)?
atom = number | variable | '(' aexpr ')' | fexpr
fexpr = function+ rexpr
rexpr = '(' aexpr ')' | number variable* | variable+
function = functionstr ('^' ('-' '1' | number | variable | '(' aexpr ')'))?
-}

{-
fVariables0 :: Parser Token Function
fVariables0 = do xs <- pStar fVariable
                 return $ FunctionProduct xs
-}

fNumber :: Parser Token Function
fNumber = do Number x <- pProp isNumber
             return $ FunctionNumber x

fVariable :: Parser Token Function
fVariable = do Variable x <- pProp isVariable
               return $ FunctionVariable x

isNumber :: Token -> Bool
isNumber (Number _) = True
isNumber _ = False

isVariable :: Token -> Bool
isVariable (Variable _) = True
isVariable _ = False

{-
isFunction :: Token -> Bool
isFunction (Function _) = True
isFunction _ = False

isSymbol :: Token -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False
-}