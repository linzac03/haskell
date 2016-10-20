--class (Functor f) => Applicative f where  
--    pure :: a -> f a  
--    (<*>) :: f (a -> b) -> f a -> f b 
--instance Applicative (a,b) where
--	pure a b = (a,b)
--	f <*> (a,b) = (f a, f b)
import Prelude hiding (Maybe(..))

newtype MyIO a = MyIO (IO a)

newtype Pair a b = Pair (a,b)
	deriving (Show)

-- Functor Programming

instance Functor (Pair t) where
	fmap f (Pair (n, m)) = Pair ((,)n (f m)) 
	
pairMap :: (b -> b) -> (a,b) -> (a,b)
pairMap f (n,m) = (n,f m)

funcComp :: (a -> b) -> ((->)t a) -> ((->)t b)
funcComp f1 f2 = f1 . f2

instance Functor MyIO where
	fmap = mapMyIO

mapMyIO :: (a -> b) -> MyIO a -> MyIO b
mapMyIO f (MyIO action) = MyIO result where
	result = do
		inp <- action
		return(f inp)

-- Pointed Programming
class Functor f => Pointed f where
	pure :: a -> f a

instance Pointed ((->)r) where
	pure x = (\_ -> x)

data Maybe a = Nothing | Just a
	deriving (Show)

instance Functor Maybe where
	fmap = mapMaybe where
 		mapMaybe f (Nothing) = Nothing
 		mapMaybe f (Just a) = Just (f a)

instance Pointed Maybe where
	pure = Just 

