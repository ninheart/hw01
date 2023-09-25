{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}

import PA1Helper
import System.Environment
import Data.Time.Format.ISO8601 (yearFormat)

-- given helper function freevars is used to extract free variables from lambda expression
freevars :: Lexp -> [String]
freevars (Atom s) = [s]
freevars (Lambda v e) = remove v (freevars e)
freevars (Apply e1 e2) = freevars e1 ++ freevars e2

-- given helper function remove as filter application using infix notation for /=
remove :: (Eq a) => a -> [a] -> [a]
remove x = filter (/= x)

-- helper function for alphaRename will start at candidate 'z' and increment 
-- until it finds the next unused variable
getFreeVar :: String -> [String] -> String
getFreeVar _ = findUnusedVar 'z'

-- helper function takes in a candidate variable and list of used variables
-- if var in usedVars: recursively calls itself with next alphabetical character
-- if var not in usedVars: returns var 
findUnusedVar :: Char -> [String] -> String
findUnusedVar var usedVars
   | [var] `elem` usedVars = findUnusedVar (nextChar var) usedVars
   | otherwise = [var]

-- Helper function to get the next character in the alphabet starting from z
-- then wraps around to 'a'
nextChar :: Char -> Char
nextChar c
   | c == 'z' = 'a'
   | otherwise = succ c

-- helper function will replace all occurence of oldVar with newVar in provided Lexp
replaceVar :: String -> String -> Lexp -> Lexp
replaceVar oldVar newVar (Atom lexp)
    | lexp == oldVar = Atom newVar
    | otherwise = Atom lexp
replaceVar oldVar newVar (Lambda var body)
   | var == oldVar = Lambda newVar (replaceVar oldVar newVar body)
   | otherwise = Lambda var (replaceVar oldVar newVar body)
replaceVar oldVar newVar (Apply expr1 expr2) =
    Apply (replaceVar oldVar newVar expr1) (replaceVar oldVar newVar expr2)

-- alpha renaming
-- 
alphaRename :: Lexp -> [String] -> Lexp
alphaRename (Lambda var body) usedVars =
         Lambda (getFreeVar var usedVars) . replaceVar var unusedVar $ body
    where
         unusedVar = getFreeVar var usedVars
alphaRename lexp _ = lexp

-- eta conversion
etaConvert :: Lexp -> Lexp
etaConvert (Lambda x (Apply f (Atom y))) =
   if x == y && notElem x (freevars f)
      then etaConvert f
        else Lambda x (Apply (etaConvert f) (Atom y))
etaConvert (Apply expr1 expr2) = Apply (etaConvert expr1) (etaConvert expr2)
etaConvert (Lambda x expr) = Lambda x (etaConvert expr)
etaConvert var@(Atom _) = var


--    For variables no beta reduction is done
--    For abstractions you beta reduce second argument
--    For applications you beta reduce both arguments and check for alpha renaming
beta :: String -> Lexp -> Lexp -> Lexp
beta var body a@(Atom z)
   | var == z = body
   | otherwise = a
beta var body (Apply f g) = Apply (beta var body f ) (beta var body g)
beta x1 y1 f@(Lambda x2 y2) =
    case () of
        _
            | x2 /= x1 && x2 `elem` freevars y1 ->
                let
                    alphaRenamed = alphaRename f (freevars y1)
                in
                    beta x1 y1 alphaRenamed
            | x2 /= x1 && x2 `notElem` freevars y1 ->
                let
                    betaY2 = beta x1 y1 y2
                in
                    Lambda x2 betaY2
            | otherwise -> f

-- check if the expression is in its simple form
simplifier :: Lexp -> Lexp -> Lexp
simplifier x y = 
   if x == y 
      then y 
      else reducer y

-- kicks off the reduction process
reducer :: Lexp -> Lexp
reducer lexp@(Atom _) = lexp
reducer lexp@(Apply f@(Lambda x y) v) = simplifier lexp (beta x (reducer v) y)
reducer lexp@(Apply x y) = simplifier lexp (Apply (reducer x) (reducer y))
reducer lexp@(Lambda x (Apply y v)) = simplifier lexp (etaConvert (Lambda x (Apply y v)))
reducer lexp@(Lambda x e) = simplifier lexp (Lambda x (reducer e))

-- read command line arguments
main = do
      args <- getArgs
      let inFile = case args of { x:_ -> x; _ -> "input.lambda" }
      let outFile = case args of { x:y:_ -> y; _ -> "output.lambda"}
      runProgram inFile outFile reducer
