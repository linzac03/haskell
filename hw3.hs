import Data.Char
-- 5.15 --
-- [0, 0.1 .. 1] would evaluate as [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
-- ghci evaluated it as [0.0,0.1,0.2,0.30000000000000004,0.4000000000000001,0.5000000000000001,0.6000000000000001,0.7000000000000001,0.8,0.9,1.0]

-- 5.16 --
-- [2, 3] has 2 items
-- [[2, 3]] has 1 item and has the type [[Int]]

-- 5.17 --
-- [2 .. 2] is evaluated as [2]
-- [2, 7 .. 2] is also evaluated as [2]

-- 5.18 --
doubleAll :: [Integer] -> [Integer]
doubleAll [] = []
doubleAll (x:xs) = [x * 2] ++ doubleAll xs

-- 5.19 --
capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs)
	| isUpper x 	= [x] ++ capitalize xs
	| otherwise		= [toUpper x] ++ capitalize xs

capitalize' :: String -> String
capitalize' "" = ""
capitalize' a = [toUpper n | n<-a] 


-- 5.20 --
divisors :: Integer -> [Integer]
divisors x = [n | n<-[1 .. x], x `mod` n == 0 ] 

isPrime :: Integer -> Bool
isPrime x
	| length (divisors x) > 2	= False
	| otherwise					= True

-- 5.21 --
matches :: Integer -> [Integer] -> [Integer]
matches _ [] = []
matches a xs = [n | n<-xs, n == a]

elem :: Integer -> [Integer] -> Bool
elem a xs
	| length (matches a xs) > 0		= True
	| otherwise						= False

-- 5.22 --
onSeperateLines :: [String] -> String
onSeperateLines [] = ""
onSeperateLines (x:xs) = x ++ "\n" ++ onSeperateLines xs 

-- 5.28 --
type Person = String
type Book = String
data Loan = Loan Person Book

type Database = [(Person, Book)]

exampleBase :: Database 
exampleBase = [ ("Alice", "Tintin"), ("Anna", "Little Women"), ("Alice", "Asterix"), ("Rory", "Tintin") ]

books :: Database -> Person -> [Book]
books db p = [snd n | n <- db, fst n == p]

borrowers :: Database -> Book -> [Person]
borrowers db b = [fst n | n <- db, snd n == b]

borrowed :: Database -> Book -> Bool
borrowed db b = length (borrowers db b) > 0

numBorrowed :: Database -> Person -> Int
numBorrowed db p = length (books db p)

-- groupByN --
groupByN :: Int -> [a] -> [[a]]
groupByN _ [] = []
groupByN n xs = [take n xs] ++ groupByN n (drop n xs)

-- listTrim --
listTrim :: (Eq a) => [a] -> [a] -> [a]
listTrim a [] = a
listTrim [] _ = []
listTrim (a:as) (b:bs)
	| a /= b	= [a] ++ listTrim as ([b] ++ bs)
	| a == b	= listTrim as bs

-- listDiff --
--listDiff :: (Eq a) => [a] -> [a] -> [a]

