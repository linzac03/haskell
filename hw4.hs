-- 7.8 --
elemNum :: Integer -> [Integer] -> Integer
elemNum _ [] = 0
elemNum m (x:xs)
	| m == x	= 1 + elemNum x xs
	| otherwise = 0 + elemNum x xs

elemNum' :: Integer -> [Integer] -> Integer
elemNum' n xs = foldl (\ m p -> if p == n then 1+m else m) 0 xs  

-- 7.9 --
-- why did i do this...
unique :: [Integer] -> [Integer]
unique [] = []
unique l@(x:xs)
	| (length $ takeWhile (==x) $ isort l []) > 1	= unique $ dropWhile (==x) $ isort l []

	| otherwise									= [x] ++ unique xs

unique' :: [Integer] -> [Integer]
unique' xs = foldl (\ m n -> if (elemNum' n xs) > 1 then m else [n] ++ m) [] xs

insert :: Integer -> [Integer] -> [Integer]
insert a [] = [a]
insert a (x:xs)
	| a <= x	= a:x:xs
	| otherwise	= x:insert a xs

isort :: [Integer] -> [Integer] -> [Integer]
isort [] s = s
isort (x:xs) s = isort xs s'
	where
		s'	= insert x s 

-- 7.10 --


-- 10.9 --
iter :: Integer -> (Integer -> Integer) -> Integer -> Integer
iter 0 f x = x
iter n f x = iter (n-1) f (f x)

--unIter :: (Integer -> Integer) -> Integer -> [Integer]
--unIter f n = unfoldr 


-- 11.17 --
curry3 :: ((a,b,c) -> d) -> (a -> b -> c -> d)
curry3 f x y z = f (x,y,z)

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (x,y,z) = f x y z

-- 11.18 --
--curryList :: ([a] -> d) -> (a -> [a] -> d)
--curryList f n l@(x:xs) = f l 

-- composeList -- 
composeList :: [(a -> a)] -> (a -> a)
composeList l@(x:xs) = if length l > 1 then (x . composeList xs) else x

composeList' :: [(a -> a)] -> (a -> a)
composeList' l@(x:xs) = foldr (.) i l
	where 
		i n = n

-- group --
group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = [[x] ++ takeWhile (==x) xs] ++ (group . dropWhile (==x)) xs

-- groupBy --
groupBy :: (a->a->Bool) -> [a] -> [[a]]
groupBy c [] = []
groupBy c l@(x:xs)
	| length xs == 0 = [[x]]
	| c x (head xs)  = [[x] ++ takeWhile (c x) xs] ++ (groupBy c $ dropWhile (c x) xs)
	| otherwise		= [[x]] ++ groupBy c xs

-- 6.44 --


