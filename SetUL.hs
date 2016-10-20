-- Set ADT Signature consistent with Data.Set
-- Note the name clashes with Prelude require qualified references
--
-- null :: Set a -> Bool                     -- is this the empty set?
-- member  :: Ord a => a -> Set a -> Bool    -- is this a member of the set?
-- empty :: Set a                            -- create and empty set.
-- fromList :: Ord a => [a] -> Set a         -- create a set from a list
-- toList :: Set a -> [a]                    -- convert a set to a list
-- insert  :: Ord a => a -> Set a -> Set a
-- delete  :: Ord a => a -> Set a -> Set a
--(Set, null, member, empty, fromList, toList, insert, delete)
module SetUL(MySet, null, member, empty, fromList, toList, insert, delete) where

import Prelude hiding (null)

newtype MySet a = MySet [a]
	deriving (Show)	

instance Eq a => Eq (MySet a) where
	(==) m1 m2 = all (`elem` (toList m2)) (toList m1)
	(/=) m1 m2 = not $ all (`elem` (toList m2)) (toList m1)

null :: MySet a -> Bool
null (MySet []) = True
null _ = False

member :: Ord a => a -> MySet a -> Bool
member n (MySet []) = False
member n (MySet ls@(x:xs))
	| n /= x	= member n (MySet xs)
	| otherwise = True  

empty :: MySet a
empty = MySet []

fromList :: Ord a => [a] -> MySet a
fromList [] = empty
fromList ls@(x:xs) = MySet ls

toList :: MySet a -> [a]
toList (MySet ls) = ls

removeDupli :: Ord a => MySet a -> MySet a
removeDupli set@(MySet ls) =  fromList (rem (toList set))
	where 
		rem [] = []
		rem ls@(x:xs)
			| x `elem` xs	= rem xs
			| otherwise		= [x] ++ rem xs

insert :: Ord a => a -> MySet a -> MySet a
insert n (MySet ls) = removeDupli $ MySet (ls ++ [n])

delete :: Ord a => a -> MySet a -> MySet a
delete n (MySet ls) = removeDupli $ MySet (takeWhile (/=n) ls ++ tail (dropWhile (/=n) ls))
