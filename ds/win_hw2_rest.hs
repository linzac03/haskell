import Data.Char

sumtwo = do
	num1 <- getLine
	num2 <- getLine
	putStr "Sum is: "
	return(read num1 + read num2)

-----------------------------------------------
putNtimes :: Integer -> String -> IO ()
putNtimes n str = do
	if n > 0 
	then putStrLn str
	else return ()
	putNtimes (n-1) str

----------------------------------------------
nSum = do
	putStrLn "Give this guy your goodies"
	num <- getLine
	putStrLn "No thank you I am gluten intolerant"
	let limit = read num
	nSumhelp limit 0
	putStrLn "Done"

nSumhelp limit n = do
	if limit == 0
	then putStrLn ("T Goddies: " ++ show n)
	else do
		putStrLn "Gimme one tho"
		num <- getLine
		nSumhelp (limit -1) (n + read num)

---------------------------------------------
wc = do
	putStrLn "Say sweet words to me"
	let count a b c = do
		str <- getLine
		if str == ""
		then putStrLn ("Number of lines read: " ++ show a 
			++ " Number of words read: " ++ show b
			++ " Number of chars read: " ++ show c)
		else do
			count (a+1) (b+cntwd str) (c+cntchr str)
				where
					cntwd s = length $ words s
					cntchr s = sum $ fmap length $ words s
	count 0 0 0 
---------------------------------------------
rmvspc [] = []
rmvspc (x:xs) 
	| x /= ""	= x:rmvspc xs
	| otherwise = rmvspc xs

ispalin = do
	putStrLn "give me word i'll tell u"
	str <- getLine
	let palin s = do
		let ls = rmvspc [if x `elem` ".,'?:;<>/[]{}-()! " then "" else [toLower x] | x <- s]
		putStrLn $ show $ rmvspc ls
		putStrLn $ "It is " ++ show (ls == reverse ls)
	palin str
	
---------------------------------------------
contpalin = do
	str <- getLine
	let palin s = do
		let ls = rmvspc [if x `elem` ".,'?:;<>/[]{}-()! " then "" else [toLower x] | x <- s]
		putStrLn $ show $ rmvspc ls
		putStrLn $ "It is " ++ show (ls == reverse ls)
	if str == ""
	then putStrLn "done"
	else do 
		palin str
		contpalin
---------------------------------------------
contsum = do
	let mysum n = do
		putStr "gimme nums: "
		num <- getLine
		if read num == 0
		then putStrLn $ show n
		else do
			mysum (n + read num)
	mysum 0

---------------------------------------------
sortedlist = do
	putStr "Give initial value for list: "
	init <- getLine
	let insert ls = do
		putStrLn $ show (ls::[Int])
		putStr "Add another number (0 to exit): "
		num <- getLine
		if read num == 0
		then putStrLn $ show (ls::[Int])
		else do
			insert ([y | y <- ls, y <= read num] ++ [read num] ++ 
					[y | y <- ls, y > read num])
	insert [read init]
		
-- 8.18 insertion sort DUHHHHHHHH	
