
Only used in the measurements:

>	class Monoid a where
>		id :: a
>		binop :: a -> a -> a

Reduce is a class that had a reducer and reducel similar to
a foldr and foldl:

>	class Reduce f where
>		reducer :: (a -> b -> b) -> (f a -> b -> b)
>		reducel :: (b -> a -> b) -> (b -> f a -> b)

Lists are instances of Reduce

>	instance Reduce [] where
>		reducer (f') x z = foldr f' z x
>		reducel (f'') x z = foldl f'' x z


*Main> reducel (\x -> \y -> y:x) [] [1,2,3,4]
[4,3,2,1]
*Main>


Once we have an instance of Reduce, we can use it to produce
a list:

>	toList :: (Reduce f) => f a -> [a]
>	toList s = s `cons'` [] where (cons') = reducer (:)

>	toDigit :: (Reduce f) => f a -> Digit a
>	toDigit s = s `dons'` E where (dons') = reducer dons

Here is a toList of a fingerTree. Since fingerTrees are instances
of Reduce, they have the functions reducer and reducel which
traverse the tree and apply the function. 

*Main> toList t1
[1,2,3,4,5,6,7,8]
*Main> toList [1,2,3,4]
[1,2,3,4]
*Main> toList (Node2 4 5)
[4,5]
*Main>


So now are some trees, with a 2 and 3 Node.


>	data Tree a = Zero a | Succ (Tree (Node a))
>		deriving Show
>	data Node a = Node2 a a | Node3 a a a
>		deriving Show

Now we have a FingerTree which keeps Digits at its leaves, 

>	data FingerTree a = Empty
>						| Single a
>						| Deep (Digit a) (FingerTree (Node a)) (Digit a)
>							deriving Show

Represent Digits as a list, for simplicity:

	type Digit a = [a]

Nodes are instances of reduce, as an example I can reduce 

*Main> (reducer (&&) (Node2 False False)) False
False
*Main> (reducer (&&) (Node2 True True)) True
True
*Main> (reducer (||) (Node2 True True)) True
True
*Main> (reducer (||) (Node2 True True)) False
True
*Main> (reducer (||) (Node2 False False)) False
False
*Main>


>	instance Reduce Node where
>		reducer lt (Node2 a b) z = a `lt` (b `lt` z)
>		reducer lt (Node3 a b c) z = a `lt` (b `lt` (c `lt` z))
>		reducel gt z (Node2 b a) = (z `gt` b) `gt` a
>		reducel gt z (Node3 c b a) = ((z`gt`c) `gt` b) `gt` a

>	instance Reduce Digit where
>		reducer lt (One a) z = a `lt` z
>		reducer lt (Two a b) z = a `lt` (b `lt` z)
>		reducer lt (Three a b c) z = a `lt` (b `lt` (c `lt` z))
>		reducer lt (Four a b c d) z = a `lt` (b `lt` (c `lt` (d `lt` z)))
>		reducel gt z (One a) = z `gt` a
>		reducel gt z (Two a b) = (z `gt` b) `gt` a
>		reducel gt z (Three a b c) = ((z `gt` c) `gt` b) `gt` a
>		reducel gt z (Four a b c d) = (((z `gt` d) `gt` c) `gt` b) `gt` a

Similarly I can reduce a finger tree:
*Main> reducer (:) t1 []
[1,2,3,4,5,6,7,8]
*Main> t1
Deep [1,2,3,4] (Single (Node3 5 6 7)) [8]
*Main>

>	instance Reduce FingerTree where
>		reducer lt Empty z = z
>		reducer lt (Single x) z = x `lt` z
>		reducer lt (Deep pr m sf) z = (reducer lt pr  ((reducer (reducer lt)) m  (reducer lt sf  z)))
>		reducel gt z Empty = z
>		reducel gt z (Single x) = z `gt` x
>		reducel gt z (Deep pr m sf) = (reducel gt (reducel (reducel gt) (reducel gt z pr) m) sf)



Deque operations (O(1))

First adding a new element on to the left of a sequence unless the
initial buffer already has 4 elements ... in that case we need to split.

>	dons :: a -> Digit a -> Digit a
>	dona a E = One a
>	dons a (One b) = Two a b
>	dons a (Two b c) = Three a b c
>	dons a (Three b c d) = Four a b c d

> 	dappend :: Digit a -> a -> Digit a
>	dappend E a = One a
> 	dappend (One a) b = Two a b
> 	dappend (Two a b) c = Three a b c
> 	dappend (Three a b c) d = Four a b c d



>	insr :: a -> FingerTree a -> FingerTree a
>	insr a  Empty	= Single a
>	insr a (Single b) = Deep (One a) Empty (One b)
>	insr a (Deep (Four b c d e) m sf) = Deep (Two a b) (insr (Node3 c d e) m) sf
>	insr a (Deep pr m sf) = Deep (a `dons` pr) m sf

>	insl :: FingerTree a -> a  -> FingerTree a
>	insl Empty a	= Single a
>	insl (Single b) a = Deep (One b) Empty (One a)
>	insl (Deep pr m (Four e d c b)) a = Deep pr (insl m (Node3 e d c)) (Two b a)
>	insl (Deep pr m sf) a = Deep pr m (sf `dappend` a)

I can lift insr and insl to make finger trees from Reduce instances:


*Main> liftinsr [1,2,3,4] Empty
Deep [1,2,3] Empty [4]
*Main> liftinsr [1,2,3,4,5,6,7] Empty
Deep [1,2,3] (Single (Node3 4 5 6)) [7]
*Main>

This takes a list to a fingerTree

>	liftinsr :: (Reduce f) => f a -> FingerTree a -> FingerTree a
>	liftinsr = reducer insr
>	liftinsl :: (Reduce f) => FingerTree a -> f a -> FingerTree a
>	liftinsl = reducel insl

>	toTree :: (Reduce f) => f a -> FingerTree a
>	toTree s = liftinsr s Empty

ViewL and ViewR are the extracts from left and right.

>	data ViewL s a = NilL | ConsL a (s a)
>		deriving Show

>	dhead :: Digit a -> a
>	dhead (One a) = a
> 	dhead (Two a b) = a
>	dhead (Three a b c) = a
>	dhead (Four a b c d) = a

>	dtail :: Digit a -> Digit a
>	dtail (One a) = E
>	dtail (Two a b) = One b
>	dtail (Three a b c) = Two b c
> 	dtail (Four a b c d) = Three b c d

>	viewL :: FingerTree a -> ViewL FingerTree a
>	viewL Empty = NilL
>	viewL (Single x) = ConsL x Empty
>	viewL (Deep pr m sf) = ConsL (dhead pr) (deepL (dtail pr) m sf)

>	deepL :: Digit a -> FingerTree (Node a) -> Digit a -> FingerTree a
>	deepL E m sf = case viewL m of 
>					NilL -> toTree sf
>					ConsL a m' -> Deep (toDigit a) m' sf
>	deepL pr m sf = Deep pr m sf

>	isEmptyL :: FingerTree a -> Bool
>	isEmptyL x = case viewL x of
>						NilL -> True
>						ConsL _ _ -> False

>	headL :: FingerTree a -> a
>	headL x = case viewL x of ConsL a _ -> a

>	tailL :: FingerTree a -> FingerTree a
>	tailL x = case viewL x of ConsL _ x' -> x'


1. Complete the mirror image of view.

>	data ViewR s a = NilR | ConsR (s a) a
>		deriving Show

>	viewR :: FingerTree a -> ViewR FingerTree a
>	viewR Empty = NilR
>	viewR (Single x) = ConsR Empty x
>	viewR (Deep pr m sf) = ConsR (deepR pr m (dtail sf)) (dhead sf) 

>	deepR :: Digit a -> FingerTree (Node a) -> Digit a -> FingerTree a
>	deepR pr m E = case viewR m of 
>					NilR -> toTree pr
>					ConsR m' a -> Deep pr m' (toDigit a)
>	deepR pr m sf = Deep pr m sf

>	isEmptyR :: FingerTree a -> Bool
>	isEmptyR x = case viewR x of
>						NilR -> True
>						ConsR _ _ -> False

>	headR :: FingerTree a -> a
>	headR x = case viewR x of ConsR _  a -> a

>	tailR :: FingerTree a -> FingerTree a
>	tailR x = case viewR x of ConsR x' _ -> x'

2. Create finger trees from lists:
         [1], [1,2,3,4]

*Main> toTree [1]
Single 1
*Main> toTree [1,2,3,4]
Deep [1,2,3] Empty [4]

3. Treating your finger tree as a deque, use insL and insR to add
to the front and the rear of the deque.

*Main> insl (toTree [1,2,3,4]) 5
Deep [1,2,3] Empty [4,5]
*Main> insr 0 (toTree [1,2,3,4])
Deep [0,1,2,3] Empty [4]

4. Use the viewL and viewR to "dequeue" elements from the left and right
respectively.

> 	dequeuer :: FingerTree a -> (FingerTree a, a)
> 	dequeuer Empty = error "Empty FingerTree"
> 	dequeuer (Single x) = (Empty, x)
> 	dequeuer (Deep pr m sf) = deqr $ viewR (Deep pr m sf)
>		where
>			deqr (ConsR ft d) = (ft, d)

> 	dequeuel :: FingerTree a -> (a, FingerTree a)
> 	dequeuel Empty = error "Empty FingerTree"
> 	dequeuel (Single x) = (x, Empty)
> 	dequeuel (Deep pr m sf) = deql $ viewL (Deep pr m sf)
>		where
>			deql (ConsL d ft) = (d, ft)

*Main> dequeuel (toTree [1,2,3,4])
(1,Deep [2,3] Empty [4])
*Main> dequeuer (toTree [1,2,3,4])
(Deep [1,2] Empty [3],4)

5. Make a fingerTree of nodes using toTree and the Node constructors.

*Main> toTree [Node2 3 4, Node3 5 6 7, Node2 1 2]
Deep [Node2 3 4,Node3 5 6 7] Empty [Node2 1 2]

6. In the paper, digits are represented as lists. Instead, use the
data type for Exercise 1: 

>	data Digit a = One a
>				   | Two a a
>				   | Three a a a
>				   | Four a a a a
>				   | E
>		deriving Show

Rework the defintions.
*Main> toTree [Node2 3 4, Node3 5 6 7, Node2 1 2]
Deep (Two (Node2 3 4) (Node3 5 6 7)) Empty (One (Node2 1 2))


