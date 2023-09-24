
import PA1Helper
import System.Environment

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


-- beta reduction
-- variable: no reduction 
-- abstractions: Lambda (x expr), reduce expressions
-- applications: (Apply expr1 , expr2) 
betaReduce :: Lexp -> Lexp
betaReduce v@(Atom _) = v
-- recursively apply beta reduction
betaReduce lexp@(Lambda x expr) = Lambda x (betaReduce expr)
-- apply beta reduction to both expressions
betaReduce lexp@(Apply expr1 expr2) =
        let
			expr3 = betaReduce expr1
			expr4 = betaReduce expr2
        -- check if alpha renaming is needed
        -- if needed, rename the variable
        	-- expr5 = alphaRename 
        -- check if expr4 is a lambda abstraction 
        in
		case expr4 of


-- eta conversion
etaConvert :: Lexp -> Lexp
-- etaConvert v@(Atom _) = v
etaConvert (Lambda x (Apply f (Atom y))) = 
   --  if x == y && not (x `elem` (map (\(Atom x) -> x) (findFreeVariables f))) 
   if x == y && notElem x (freevars f)     
      then etaConvert f
        else Lambda x (Apply (etaConvert f) (Atom y))
etaConvert (Apply expr1 expr2) = Apply (etaConvert expr1) (etaConvert expr2)
etaConvert (Lambda x expr) = Lambda x (etaConvert expr)
etaConvert var@(Atom _) = var


start :: Lexp -> Lexp
start lexp = etaConvert (betaReduce lexp)

-- read command line arguments
main = do
        args <- getArgs
        let inFile = case args of { x:_ -> x; _ -> "input.lambda" }
        let outFile = case args of { x:y:_ -> y; _ -> "output.lambda"}
        runProgram inFile outFile start
