

------Part 1
--data Maybe a = Nothing | Just a
--	deriving Show

data Expr = Val Int | Var String | BinOp (Int -> Int -> Int) Expr Expr

type Env = [(String, Int)]

fetch x env = head' [b | (a,b) <- env, x == a]
	where
		head' [] = error "Variable does not exist in environment"
		head' (x:xs) = x

infixl 4 <*>
class Functor f => Applicative f where
	pure :: a -> f a
	(<*>) :: f (a -> b) -> f a -> f b

--instance Monad Maybe where
--	return 		   = Just 
--	(Just x) >>= g = g x
--	Nothing >>= _  = error "Encountered a Nothing"

evalM :: Expr -> Env -> Int
evalM (Val x)  	= return x 
evalM (Var v) 	= fetch v 
evalM (BinOp op e1 e2) = do n <- evalM e1;
						   	m <- evalM e2;
						   	return (op n m)

----Part 2
--instance Monad ((->) r) where
--	return g = \r -> g
--	eg >>= ex = \r -> ex (eg r) r

evalR :: Expr -> Env -> Int
evalR (Val x) 	= return x
evalR (Var v) 	= fetch v
evalR (BinOp op e1 e2) = do x <- evalR e1;
							y <- evalR e2;
							return (op x y)

-----Part 3
evalRM :: Expr -> Env -> Maybe Int
evalRM (Val x) = return (return x)
evalRM (Var v) = (\ env -> lookup v env)
evalRM (BinOp op e1 e2) = do
						  x <- evalRM e1
						  y <- evalRM e2
						  return (lift' op x y)
	where
		lift' op (Just a) (Just b) = Just (op a b)
		lift' op Nothing _ = Nothing
