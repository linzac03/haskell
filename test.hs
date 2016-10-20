
double x = x * 2

frx f z [] = z
frx f z (x:xs) = f x (frx f z xs)

fr1x f [x] = x
fr1x f (x:xs) = f x (fr1x f xs)
fr1x _ [] = error "Prelude.foldr1: empty list"

unfold :: (Eq a, Num a) => ([a] -> Bool) -> ([a] -> a) -> ([a] -> [a]) -> [a] -> [a]
unfold p h t x 
	| p x		= []
	| otherwise = h x : unfold p h t (t x)

