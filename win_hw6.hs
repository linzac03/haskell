-- file: JoinAndFishMonads.hs
-- Neal Nelson
-- 2013.02.03 Monad defined with Join instead of Bind
-- 2013.02.05 Monad defined with Fish instead of Bind

import Prelude hiding (Maybe(..),Either(..))

data Maybe a = Nothing | Just a
	deriving Show

data Either a b = Left a | Right b
	deriving Show
------------------------------------------------------------------------------
-- The Applicative Class
------------------------------------------------------------------------------
infixl 4 <*>,<$>
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- Utility functions for Applicative
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 g x y = pure g <*> x <*> y
--             = g <$> x <*> y
--             = fmap g x <*> y


-- Instances of the Applicative Class

-- The ((->) r) Reader instance of the Applicative Class for evaluation
-- in an Environment

-- instance Functor ((->) env) where
--   fmap = (.)

-- instance Applicative ((->) env) where
--   pure g = \env -> g
--   eg <*> ex = \env -> (eg env) (ex env)


-- The Maybe instance of the Applicative Class for failure propogation
-- This Functor instance for Maybe is already in the Prelude

instance Functor Maybe where
  fmap g Nothing  = Nothing
  fmap g (Just x) = Just (g x)

instance Applicative Maybe where
  pure = Just
  Nothing <*> _  = Nothing
  (Just g) <*> x = fmap g x      -- note the shortcut using fmap

  
-- Exercise
-- instance Applicative (Either a)
instance Functor (Either a) where
	fmap f (Left a) = Left a
	fmap f (Right b) = Right (f b)

instance Applicative (Either a) where
	pure = Right
	(Right g) <*> x = fmap g x

-- Exercise
-- Utility functions for the Applicative class
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x
-- sequence
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

-- mapM 
mapA :: (Applicative m) => (a -> m b) -> [a] -> m [b]
mapA f = sequenceA . fmap f

------------------------------------------------------------------------------
-- MyMonad class
------------------------------------------------------------------------------

class Applicative m => MyMonad m where
  bind :: m a -> (a -> m b) -> m b
  returnm :: a -> m a

instance MyMonad Maybe where
  bind (Just x) g = g x
  bind Nothing  _ = Nothing
  returnm = Just

instance MyMonad (Either a) where
  bind (Right x) g = g x
  returnm = Right

-- Utility functions for MyMonad

-- myap
myap :: MyMonad m => m (a -> b) -> m a -> m b
myap m1 m2 = liftmm id m1 m2 
	where 
		liftmm f m1 m2 = m1 `bind` (\x -> 
						 m2 `bind` (\y ->
						 returnm (f x y))) 
  
-- mysequence
mysequence :: MyMonad m => [m a] -> m [a]
mysequence [] = pure []
mysequence (x:xs) = (:) <$> x <*> mysequence xs

-- myMapM
myMapM :: MyMonad m => (a -> m b) -> [a] -> m [b]
myMapM f = mysequence . map f
 
-- fish
infixr 1 >=>
(>=>) :: MyMonad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \n -> f n `bind` g


-- join
joinM :: MyMonad m => m (m a) -> m a
joinM x = x `bind` id



------------------------------------------------------------------------------
-- JoinMonad class
------------------------------------------------------------------------------
class Applicative m => JoinMonad m where
  join :: m (m a) -> m a
  returnjm :: a -> m a

-- Exercise
-- JoinMonad Laws Listed
  
instance JoinMonad Maybe where
  join (Just x) = x
  join _        = Nothing
  returnjm 		= Just

-- Exercise
instance JoinMonad (Either a) where
  join (Right x) = x
  returnjm 		 = Right

-- Exercise
-- JoinMonad Laws satisfied for (Either a)

-- Utility Functions for JoinMonad

bindWithJoin :: JoinMonad m => m a -> (a -> m b) -> m b
bindWithJoin m g = join (fmap g m)

-- ap
joinap :: JoinMonad m => m (a -> b) -> m a -> m b
joinap m1 m2 = liftjm id m1 m2 
	where 
		liftjm f m1 m2 = m1 `bindWithJoin` (\x -> 
						 m2 `bindWithJoin` (\y ->
						 returnjm (f x y))) 
-- sequence
joinsequence :: JoinMonad m => [m a] -> m [a]
joinsequence [] = pure []
joinsequence (x:xs) = (:) <$> x <*> joinsequence xs

-- mapM
joinmapM :: JoinMonad m => (a -> m b) -> [a] -> m [b]
joinmapM f = joinsequence . map f

-- bind

-- fishWithJoin
fishWithJoin :: JoinMonad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f `fishWithJoin` g = \n -> f n `bindWithJoin` g


------------------------------------------------------------------------------
-- FishMonad class (left to right fish (>=>) )
------------------------------------------------------------------------------
class Applicative m => FishMonad m where
  fish :: (a -> m b) -> (b -> m c) -> (a -> m c)
  
-- Exercise
-- FishMonad Laws Listed
  
instance FishMonad Maybe where
  fish g h = \x -> case (g x) of
                     Nothing  -> Nothing
                     (Just y) -> h y

-- 9. Exercise
instance FishMonad (Either a) where
  fish g h = \x -> case (g x) of
				     Right y -> h y

-- Exercise
-- FishMonad Laws satisfied for (Either a)


-- 10. Utility Functions for FishMonad

bindWithfish :: FishMonad m => m a -> (a -> m b) -> m b
bindWithfish m g = fish (\x -> m) g id  -- where do I get z of type a?

-- joinWithFish

