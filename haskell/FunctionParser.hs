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
                FunctionInverse Function |
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
rexpr = '-'? ('(' aexpr ')' | number variable* | variable+)
function = functionstr ('^' ('-' '1' | number | variable | '(' aexpr ')'))?
-}

fJExpr :: Parser Token Function
fJExpr = do s <- pMaybe $ pElt $ Symbol '-'
            ts <- pPlus fEExpr
            let u = case s of
                  Nothing -> ts
                  Just _ -> FunctionNumber (negate 1.0) : ts
            return $ FunctionProduct u

fEExpr :: Parser Token Function
fEExpr = do b <- fAtom
            e <- pMaybe (pElt (Symbol '^') >> fJExpr)
            case e of
              Nothing -> return b
              Just ee -> return $ FunctionPower b ee

-- TODO: This needs '(' aexpr ')' added as an alternative
fAtom :: Parser Token Function
fAtom = fNumber ||| fVariable ||| fFExpr

fFExpr :: Parser Token Function
fFExpr = do fs <- pPlus fFunction
            arg <- fRExpr
            -- I'm not sure if the next line is beautiful or horrible,
            -- but it's definitely Haskell...
            return $ foldr ($) arg fs

fFunction :: Parser Token (Function -> Function)
fFunction = do f <- fFunctionStr
               p <- pMaybe fFunctionRaisedTo
               case p of
                 Nothing -> return f
                 Just pp -> return $ pp . f

fFunctionRaisedTo :: Parser Token (Function -> Function)
fFunctionRaisedTo = do pElt $ Symbol '^'
                       fFunctionRaisedToPower ||| fFunctionRaisedToInverse

-- TODO: This needs '(' aexpr ')' added as an alternative
fFunctionRaisedToPower :: Parser Token (Function -> Function)
fFunctionRaisedToPower = do r <- fNumber ||| fVariable
                            return $ flip FunctionPower r

fFunctionRaisedToInverse :: Parser Token (Function -> Function)
fFunctionRaisedToInverse = do pElt $ Symbol '-'
                              pElt $ Number 1.0
                              return FunctionInverse

fFunctionStr :: Parser Token (Function -> Function)
fFunctionStr = do Function x <- pProp isFunction
                  return $ FunctionFunction x

fRExpr :: Parser Token Function
fRExpr = do s <- pMaybe $ pElt $ Symbol '-'
            r <- fRExpr'
            case s of
              Nothing -> return r
              Just _ -> return $ makeProduct [FunctionNumber $ negate 1.0, r]

-- TODO: This needs '(' aexpr ')' added as a third alternative
fRExpr' :: Parser Token Function
fRExpr' = (do x <- fNumber
              y <- fVariables0
              return $ makeProduct [x,y]) ||| fVariables1

fVariables1 :: Parser Token Function
fVariables1 = do xs <- pPlus fVariable
                 return $ makeProduct xs

fVariables0 :: Parser Token Function
fVariables0 = do xs <- pStar fVariable
                 return $ makeProduct xs

makeProduct :: [Function] -> Function
makeProduct fs = case flattenProduct fs of
  [] -> FunctionProduct []
  [f] -> f
  (FunctionNumber a : FunctionNumber b : fs) ->
    makeProduct (FunctionNumber (a*b) : fs)
  fs -> FunctionProduct fs

flattenProduct :: [Function] -> [Function]
flattenProduct [] = []
flattenProduct (FunctionProduct f:fs) = flattenProduct f ++ flattenProduct fs
flattenProduct (f:fs) = f : flattenProduct fs

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

isFunction :: Token -> Bool
isFunction (Function _) = True
isFunction _ = False

{-
isSymbol :: Token -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False
-}