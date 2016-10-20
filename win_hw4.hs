import Prelude hiding (Maybe(..))

data Maybe a = Nothing | Just a
	deriving (Eq, Show)

class Functor f => Pointed f where
	pure :: a -> f a

class Pointed f => Applicative f where
	(<*>) :: f (a -> b) -> f a -> f b

instance Pointed (Either a) where
	pure = Right

instance Applicative (Either a) where
	(<*>) (Left a) _ = Left a
	(<*>) (Right a) something = fmap a something

instance Pointed [] where
	pure = (:[])

instance Applicative [] where
	(<*>) [] _ = []
	(<*>) [f] something = fmap f something

instance Pointed ((->)r) where
	pure x = (\_ -> x)

instance Applicative ((->)r) where
	(<*>) f g = \x -> f x (g x)

instance Functor Maybe where
	fmap = mapMaybe where
 		mapMaybe f (Nothing) = Nothing
 		mapMaybe f (Just a) = Just (f a)

instance Pointed Maybe where
	pure = Just 

----------------------------------------
--proofs--
----------------------------------------

----------------------------
--instance Functor [] where
--	fmap _ [] = []
--	fmap g (x:xs) = g x : g x : fmap g xs  
--
-- fmap id [] = []
-- fmap id (x:xs) = id x : fmap id xs
-- 				  = x : xs
--
-- id x : id x : fmap id xs = x : x : xs /= x : xs
-- ------------------------------------
maybeb1 :: Eq a => Maybe a -> Bool
maybeb1 a = fmap id a == id a
maybeb2 :: Eq a => Maybe a -> Bool 
maybeb2 a = fmap (id . id) a == (fmap id . fmap id) a
--I'm not sure why but I can't define anything in terms of maybeb1 and
--maybeb2, but they are interpretted correctly and can be used in ghci.
--But I didn't need to use induction anyway.
maybef1 = fmap id (Just 1) == id (Just 1)
maybef2 = fmap (id . id) (Just 1) == (fmap id . fmap id) (Just 1)
-- fmap (g . h) (Maybe a) = Maybe ((g . h) a)
-- 						  = Maybe $ g(h a) 
-- (fmap g . fmap h) (Maybe a) = fmap g (Maybe (h a))
-- 							   = Maybe $ g(h a)

--------------------------------------
pmaybe = Just 1 == pure 1

--------------------------------------
fmaybe = fmap id (pure 1) == id (Just 1) && fmap (id.id) (pure 1) == (fmap id . fmap id) (Just 1)

-------------------------------------
newtype Pair a b = Pair (a,b)
	deriving (Show, Eq)

instance Functor (Pair t) where
	fmap f (Pair (n, m)) = Pair ((,)n (f m)) 

pairf1 (Pair (n, m)) = fmap id (Pair (n, m)) == id (Pair (n, m))
pairf2 (Pair (n, m)) = fmap (id.id) (Pair (n, m)) == (fmap id . fmap id) (Pair (n, m))

