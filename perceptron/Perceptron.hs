import System.IO.Unsafe
--Perceptron = [x1,x2,x3...xm, r]
newtype Perceptron a = Perceptron [(a,[[a]])]
	deriving (Show)

orTraining = [[0,0,0],[0,1,1],[1,0,1],[1,1,1]] :: [[Int]]
andTraining = [[0,0,0],[0,1,0],[1,0,0],[1,1,1]] :: [[Int]]


getps (Perceptron ps) = ps	

train p [] = p
train p@(Perceptron a) tset@(x:xs) = train (teach x p) xs

teach is p@(Perceptron ps) 
	| (last is) `elem` (fmap fst ps) = Perceptron (exins is ps)
	| otherwise 				   	 = Perceptron (newins is ps)
		where
			newins xs ps  = (last xs, [init xs]) : ps
			exins xs ps = ((last xs), ((init xs) : ((snd . head) $ 
							filter ((==last xs) . fst) ps))) : filter ((/=last xs) . fst) ps
										
weight :: Perceptron Int -> IO ()
weight (Perceptron ps) = guess ps []
	where 
		guess ps ws = do
			let n = 1 + (length . head . snd . head) ps
			putStrLn ("You need " ++ show n ++ " weights: ")
			if and $ evalws (ps :: [(Int,[[Int]])]) $ (unsafePerformIO $ getws ws n :: [Int])
			then putStrLn "Well done, those weights work"
			else putStrLn "Yeah that doesn't work.. exiting.."

evalws :: [(Int,[[Int]])] -> [Int] -> [Bool]
evalws [] ws = []
evalws (p:ps) ws = evalhelper (fst p, snd p) ws ++ evalws ps ws

evalhelper :: (Int,[[Int]]) -> [Int] -> [Bool]
evalhelper (out, []) _ = []
evalhelper (out, (i:ins)) (w:ws) = (\ (z,[x,y]) -> if z > 0 
												   then (w + head ws*x + last ws*y) > 0 
												   else (w + head ws*x + last ws*y) < 0) (out, i) 
												   : evalhelper (out, ins) (w:ws) 

getws :: [Int] -> Int -> IO ([Int])
getws ws 0 = do
	return (ws)
getws ws n = do
	putStr ("guess weight " ++ show (n-1) ++ ": ")
 	num <- getLine
	let r = read num
	if ws == []
	then getws [r] (n-1)
	else getws (r : ws) (n-1)

testand = weight $ train (Perceptron []) andTraining
testor = weight $ train (Perceptron []) orTraining

--you gotta guess what weights work for the whole set, thats the real learning
