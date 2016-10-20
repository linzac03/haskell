import Prelude hiding Functor(..), Applicative(..)

data Exp v = Var v | Val Int | Add (Exp v) (Exp v)

type Env = [(String, Int)]

fetch x env = head [b | (a,b) <- env, x == a]

class Functor f where
	

class Functor f => Applicative f where
	pure  :: a -> f a
	(<*>) :: f (a -> b) -> f a -> f b

instance Functor ((->) env) where
	fmap = (.)

instance Applicative ((->) env) where
	pure g = \env -> g
	eg <*> ex = \env -> (eg env) (ex env)

eval :: Exp v -> Env -> Int
eval (Var x) env   = fetch x env
eval (Val i) env   = i
eval (Add p q) env = eval p env + eval q env
