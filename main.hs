
import PA1Helper
import System.Environment


-- return a list of expressions. 
allVariables :: Lexp -> [Lexp]
allVariables v@(Atom _) = [v]
allVariables lexp@(Lambda x expr) = allVariables expr
allVariables (Apply expr1 expr2) = allVariables expr1 ++ allVariables expr2

remove :: (Eq a) => [a] -> a -> [a]
remove [] _ = []
remove list@(h:t) x
    | x == h = remove t x
    | otherwise = h: remove t x

findFreeVariables :: Lexp -> [Lexp]
findFreeVariables v@(Atom _) = [v]
findFreeVariables lexp@(Lambda x expr) = findFreeVariables expr `remove` Atom x
findFreeVariables lexp@(Apply expr1 expr2) = findFreeVariables expr1 ++ findFreeVariables expr2

alphaRename :: Lexp -> [String] -> Lexp
alphaRename v@(Atom _)_ = v


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
    if x == y && not (x `elem` (map (\(Atom x) -> x) (findFreeVariables f))) 
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
