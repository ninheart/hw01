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

-- helper function to get a free variable that is not in the list of used variables
getFreeVar :: String -> [String] -> String
getFreeVar var usedVars =
  let allVars = [c : n | n <- "" : map show [1 ..], c <- ['z', 'a' .. 'z']]
   in head [unusedVar | unusedVar <- allVars, unusedVar `notElem` (var : usedVars)]

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
-- if a Lamda expression, it creates a new Lamda expression 
-- takes in a lexp and list of usedVars and creates a (Lambda var body) 
-- where var is calculated with (getFreeVar var usedVars) and body is (replaceVar var unusedVar)
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
