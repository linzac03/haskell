
>	class Reduce f where
>		reducer :: (a -> b -> b) -> (f a -> b -> b)
>		reducel :: (b -> a -> b) -> (b -> f a -> b)


>	instance Reduce [] where
>		reducer (f') x z = foldr f' z x
>		reducel (f'') x z = foldl f'' x z

>	toList :: (Reduce f) => f a -> [a]
>	toList s = s `cons'` [] where (cons') = reducer (:)


>	data Node a = Node2 a a | Node3 a a a
>		deriving Show


>	data FingerTree a = Empty
>						| Single a
>						| Deep (Digit a) (FingerTree (Node a)) (Digit a)
>							deriving Show

>	type Digit a = [a]


>	instance Reduce Node where
>		reducer lt (Node2 a b) z = a `lt` (b `lt` z)
>		reducer lt (Node3 a b c) z = a `lt` (b `lt` (c `lt` z))
>		reducel gt z (Node2 b a) = (z `gt` b) `gt` a
>		reducel gt z (Node3 c b a) = ((z`gt`c) `gt` b) `gt` a

>	instance Reduce FingerTree where
>		reducer lt Empty z = z
>		reducer lt (Single x) z = x `lt` z
>		reducer lt (Deep pr m sf) z = (reducer lt pr  ((reducer (reducer lt)) m  (reducer lt sf  z)))
>		reducel gt z Empty = z
>		reducel gt z (Single x) = z `gt` x
>		reducel gt z (Deep pr m sf) = (reducel gt (reducel (reducel gt) (reducel gt z pr) m) sf)


>	insl :: a -> FingerTree a -> FingerTree a
>	insl a  Empty	= Single a
>	insl a (Single b) = Deep [a] Empty [b]
>	insl a (Deep [b,c,d,e] m sf) = Deep [a,b] (insl (Node3 c d e) m) sf
>	insl a (Deep pr m sf) = Deep ([a] ++ pr) m sf

>	insr :: FingerTree a -> a  -> FingerTree a
>	insr Empty a	= Single a
>	insr (Single b) a = Deep [b] Empty [a]
>	insr (Deep pr m [e,d,c,b] ) a = Deep pr (insr m (Node3 e d c))  [b,a]
>	insr (Deep pr m sf) a = Deep pr m (sf ++ [a])


>	liftinsl :: (Reduce f) => f a -> FingerTree a -> FingerTree a
>	liftinsl = reducer insl
>	liftinsr :: (Reduce f) => FingerTree a -> f a -> FingerTree a
>	liftinsr = reducel insr

>	toTree :: (Reduce f) => f a -> FingerTree a
>	toTree s = liftinsl s Empty

ViewL and ViewR are the extracts from left and right.

>	data ViewL s a = NilL | ConsL a (s a)
>		deriving Show

>	viewL :: FingerTree a -> ViewL FingerTree a
>	viewL Empty = NilL
>	viewL (Single x) = ConsL x Empty
>	viewL (Deep pr m sf) = ConsL (head pr) (deepL (tail pr) m sf)

>	deepL :: [a] -> FingerTree (Node a) -> Digit a -> FingerTree a
>	deepL [] m sf = case viewL m of 
>					NilL -> toTree sf
>					ConsL a m' -> Deep (toList a) m' sf
>	deepL pr m sf = Deep pr m sf

>	isEmptyL :: FingerTree a -> Bool
>	isEmptyL x = case viewL x of
>						NilL -> True
>						ConsL _ _ -> False

>	headL :: FingerTree a -> a
>	headL x = case viewL x of ConsL a _ -> a

>	tailL :: FingerTree a -> FingerTree a
>	tailL x = case viewL x of ConsL _ x' -> x'

Now the right side:

>	data ViewR s a = NilR | ConsR a (s a)
>		deriving Show

>	viewR :: FingerTree a -> ViewR FingerTree a
>	viewR Empty = NilR
>	viewR (Single x) = ConsR x Empty
>	viewR (Deep pr m sf) = ConsR (head sf) (deepR pr m (tail sf) ) 


>	deepR :: [a] -> FingerTree (Node a) -> Digit a -> FingerTree a
>	deepR pr m [] = case viewR m of 
>					NilR -> toTree pr
>					ConsR a m' -> Deep pr m' (toList a)
>	deepR pr m sf = Deep pr m sf

>	isEmptyR :: FingerTree a -> Bool
>	isEmptyR x = case viewR x of
>						NilR -> True
>						ConsR _ _ -> False

>	headR :: FingerTree a -> a
>	headR x = case viewR x of ConsR  a _ -> a

>	tailR :: FingerTree a -> FingerTree a
>	tailR x = case viewR x of ConsR a x' -> x'

>	t1 = toTree (take 40 [1..])
>	t2 = toTree (take 40 [50..])

>	app3 :: FingerTree a -> [a] -> FingerTree a -> FingerTree a
>	app3 Empty ts xs = ts `liftinsl` xs
>	app3 xs ts Empty = xs `liftinsr` ts
>	app3 (Single x) ts xs = x `insl` (ts `liftinsl` xs)
>	app3 xs ts (Single x) = (xs `liftinsr` ts) `insr` x
>	app3 (Deep pr1 m1 sf1) ts (Deep pr2 m2 sf2) = Deep pr1 (app3 m1 (nodes (sf1 ++ ts ++ pr2)) m2) sf2

>	nodes :: [a] -> [Node a]
>	nodes [a,b] = [Node2 a b]
>	nodes [a,b,c] = [Node3 a b c]
>	nodes [a,b,c,d] = [Node2 a b, Node2 c d]
>	nodes (a:b:c:xs) = Node3 a b c: nodes xs

>	append :: FingerTree a -> FingerTree a -> FingerTree a
>	append xs ys = app3 xs [] ys

