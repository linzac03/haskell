myLast :: (Eq a) => [a] -> a
myLast (x:xs)
	| xs == []		= x
	| otherwise		= myLast xs


myButLast :: (Eq a) => [a] -> a
myButLast (x:xs)
	| length xs == 1	= x
	| otherwise 		= myButLast xs

elementAt :: (Eq a, Num a) => [t] -> a -> t 
elementAt (x:xs) n 
	| n == 1	= x
	| otherwise = elementAt xs (n-1)

myLength :: (Eq a) => [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome (x:xs) = (x:xs) == myReverse (x:xs)

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a ) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

compress :: (Eq a) => [a] -> [a]
compress (x:xs)
	| xs == []		= [x] 
	| x == head xs	= compress xs
	| otherwise		= [x] ++ compress xs

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

--encode :: (Eq a, Num a) => [a] -> [(t,a)]
encode [] = []
encode l@(x:xs) = fmap (\ n -> (length n, head n)) (pack l) 
