-- 11 --
data ListItem a = Single a | Multiple Int a
	deriving (Show)

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified [] = []
encodeModified l@(x:xs) = fmap (\ n -> if length n > 1 then Multiple (length n) (head n) else Single (head n)) (pack l) 

-- 12 -- 
decode :: (Eq a) => [ListItem a] -> [a]
decode [] = []
decode (x:xs) = helper x ++ decode xs
	where 
		helper (Single a) = [a]
		helper (Multiple n a) = replicate n a

-- 13 --
encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x:xs)
	| x == head xs	= Multiple (length (takeWhile (==x) xs)+1) x:encodeDirect (dropWhile (==x) xs)	
	| otherwise		= Single x:encodeDirect xs

-- 14 --
dupli :: (Eq a) => [a] -> [a]
dupli [] = []
dupli (x:xs) = [x] ++ [x] ++ dupli xs  

-- 15 --
repli :: (Eq a) => [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) 0 = xs 
repli (x:xs) n = replicate n x ++ repli xs n

-- 16 --
dropEvery :: (Eq a) => [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery l a = take (a-1) l ++ dropEvery (drop a l) a

-- 17 --

