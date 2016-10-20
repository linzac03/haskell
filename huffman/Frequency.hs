module Frequency (frequency) where

mergeSort :: ([a] -> [a] -> [a]) -> [a] -> [a]
mergeSort merge xs
	| length xs < 2		= xs
	| otherwise			
		= merge (mergeSort merge first) (mergeSort merge second)
			where
				first = take half xs
				second = drop half xs
				half = (length xs) `div` 2

alphaMerge :: [(Char,Int)] -> [(Char,Int)] -> [(Char,Int)]
alphaMerge xs [] = xs
alphaMerge [] ys = ys
alphaMerge ((p,n):xs) ((q,m):ys)
	| (p==q) = (p,m+n) : alphaMerge xs ys
	| (p<q) = (p,n) : alphaMerge xs ((q,m):ys)
	| otherwise = (q,m) : alphaMerge ((p,n):xs) ys

freqMerge :: [(Char,Int)] -> [(Char,Int)] -> [(Char,Int)]
freqMerge xs [] = xs
freqMerge [] ys = ys
freqMerge ((p,n):xs) ((q,m):ys)
	| (n<m || (n==m && p<q))	= (p,n) : freqMerge xs ((q,m):ys)
	| otherwise					= (q,m) : freqMerge ((p,n):xs) ys

frequency :: [Char] -> [(Char,Int)]
frequency = mergeSort freqMerge . mergeSort alphaMerge . map start
	where 
		start ch = (ch,1)
