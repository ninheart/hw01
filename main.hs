
import PA1Helper
import System.Environment


-- Use beta reduction recurively 
-- alpha renaming
alphaRename :: Lexp -> [String] -> Lexp



-- beta reduction
betaReduce :: Lexp -> Lexp


-- eta reduction
etaConvert :: Lexp -> Lexp

reducer :: Lexp -> Lexp
reducer lexp = lexp

start :: Lexp -> Lexp
start lexp = etaConvert (betaReduce lexp)
-- read command line arguments
main = do 
	args <- getArgs
	let inFile = case args of { x:_ -> x; _ -> "input.lambda" }
	let outFile = case args of { x:y:_ -> y; _ -> "output.lambda"}
	runProgram inFile outFile reducer