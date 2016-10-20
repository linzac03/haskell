-- file: evalWithMaybe-skel.hs
-- Neal Nelson
-- Skeleton code - this code will not run as-is
-- This skeleton is for part 2 of hw5

-- Contents
-- 1. A simple evaluator using the Maybe type to propogate failure.

-- 2. The evaluator is coded by lifting binary operations to the Maybe Functor

-- 3. The evaluator is coded with both the Maybe functor and the
--    Reader ((->) r) Functor to propogate failure and also abstract away
--    from carrying an explicit environment for variables.


------------------------------------------------------------------------------
-- 1. A simple evaluator with failure propogation rather than rudely bailing
--    with Prelude exception
------------------------------------------------------------------------------

-- The evaluator assumes abstract expressions have read-only variables and only
-- binary Int operators

import GHC.Base hiding (Functor (..))

-- Abstract Syntax
data Expr = Val Int
          | Var String
          | BinOp (Int -> Int -> Int) Expr Expr


-- Environment mapping variables to values
type Env = [(String, Int)]
	
-- An environment lookup with abrupt termination on lookup failure
fetch x env = head [b | (a,b) <- env, x == a ]
	where 
		head [] = error "Variable does not exist in environment"


-- The evaluator with explicit plumbing of Nothing failure propogation
-- This version DOES NOT use applicative functors. You have to code it by hand
evalMRef :: Expr -> Env -> Maybe Int
evalMRef (Val x) env = Just x 
	where
		head [] = Nothing
		lookup Nothing _ = Nothing	
evalMRef (Var v) env          = lookup v env  -- lookup returns Maybe type
evalMRef (BinOp op e1 e2) env = maybe op (evalMRef e1 env) (evalMRef e2 env)
	where
		maybe op (Just a) (Just b) = Just (op a b)
		maybe op Nothing _		   = Nothing
		maybe op _ Nothing		   = Nothing


------------------------------------------------------------------------------
-- The simple evaluator coded by lifting to the Maybe Functor
------------------------------------------------------------------------------

-- Lift a binary operator to the Maybe Functor
lift2 :: (Int -> Int -> Int) -> Maybe Int -> Maybe Int -> Maybe Int
lift2 op (Just a) (Just b) = Just (op a b)
lift2 op Nothing _ 		   = Nothing
lift2 op _ Nothing		   = Nothing
-- fill in code. 


evalMRef2 :: Expr -> Env -> Maybe Int
evalMRef2 (Val x) env          = lookup (head [a | (a,b) <- env, x == b]) env
	where
		head [] = Nothing
		lookup Nothing _ = Nothing	
evalMRef2 (Var v) env          = lookup v env  -- lookup returns Maybe type
evalMRef2 (BinOp op e1 e2) env = lift2 op (evalMRef e1 env) (evalMRef e2 env)



------------------------------------------------------------------------------
-- The simple evaluator coded by using both the Reader and Maybe applicative
-- Functors. The Reader Functor ((->) env) abstracts away the need to
-- explicitly carry around the Env environment map.
------------------------------------------------------------------------------


-- The Applicative Class
infixl 4 <*>
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- The Reader ((->) r) instance of the Applicative Class for evaluation in an
-- environment env.
--instance Functor ((->) env) where
--  fmap = (.)

instance Applicative ((->) r) where
  pure g = \r -> g
  eg <*> ex = \r -> (eg r) (ex r)


-- The Maybe instance of the Applicative Class for failure propogation
-- This Functor instance for Maybe is already in the Prelude

--instance Functor Maybe where
--  fmap g Nothing  = Nothing
--  fmap g (Just x) = Just (g x)


instance Applicative Maybe where
  pure = Just
  Nothing <*> _  = Nothing
  (Just g) <*> x = fmap g x      -- note the shortcut using fmap

-- Code lift2 using Applicative just for fun
liftA2 :: (Int -> Int -> Int) -> Maybe Int -> Maybe Int -> Maybe Int
liftA2 g x y = pure g <*> x <*> y
--           = g <$> x <*> y
--           = fmap g x <*> y

-- The Maybe evaluator expressed using Applicative Functors 
evalM :: Expr -> Env -> Maybe Int
evalM (Val x)         		= pure (pure x)
evalM (Var v)       		= lookup v  
evalM (BinOp op e1 e2) = pure <*> pure op <*> (evalM e1) <*> (evalM e2)


------------------------------------------------------------------------------
-- Testing
------------------------------------------------------------------------------

e1 = Val 3
e2 = BinOp (+) (Val 3) (Val 4)
e3 = BinOp (*) (BinOp (-) (Val 9) (Val 4)) (BinOp (-) (Val 7) (Val 2)) 
e4 = BinOp (+) (BinOp (*) (Val 9) (Val 4)) (BinOp (div) (Val 7) (Val 2)) 
e5 = BinOp (-) (Val 3) (BinOp (-) (Val 4) (Val 6)) 
e6 = Var "a" 
e7 = BinOp (+) (Var "a") (Var "b")
e8 = BinOp (*) (BinOp (-) (Var "a") (Val 4)) (BinOp (-) (Val 7) (Var "b")) 
e9 = BinOp (+) (BinOp (*) (Val 9) (Var "a")) (BinOp (div) (Var "b") (Var "c")) 


test_evalM = [ evalM e1 [] == Just 3
              ,evalM e2 [] == Just 7
              ,evalM e3 [] == Just 25
              ,evalM e4 [] == Just 39
              ,evalM e5 [] == Just 5
              ,evalM e6 [("a",5)] == Just 5
              ,evalM e7 [("a",5), ("b",8)] == Just 13
              ,evalM e8 [("a",7), ("b",2)] == Just 15
              ,evalM e9 [("a",7), ("b",8), ("c", 2)] == Just 67
              ]
test_evalM_fail =
  evalM e9 [("a",7), ("b",8)]

test_evalMRef = [ evalMRef e1 [] == Just 3
                 ,evalMRef e2 [] == Just 7
                 ,evalMRef e3 [] == Just 25
                 ,evalMRef e4 [] == Just 39
                 ,evalMRef e5 [] == Just 5
                 ,evalMRef e6 [("a",5)] == Just 5
                 ,evalMRef e7 [("a",5), ("b",8)] == Just 13
                 ,evalMRef e8 [("a",7), ("b",2)] == Just 15
                 ,evalMRef e9 [("a",7), ("b",8), ("c", 2)] == Just 67
                 ]
test_evalMRef_fail =
  evalMRef e9 [("a",7), ("b",8)]

test_evalMRef2 = [ evalMRef2 e1 [] == Just 3
                 ,evalMRef2 e2 [] == Just 7
                 ,evalMRef2 e3 [] == Just 25
                 ,evalMRef2 e4 [] == Just 39
                 ,evalMRef2 e5 [] == Just 5
                 ,evalMRef2 e6 [("a",5)] == Just 5
                 ,evalMRef2 e7 [("a",5), ("b",8)] == Just 13
                 ,evalMRef2 e8 [("a",7), ("b",2)] == Just 15
                 ,evalMRef2 e9 [("a",7), ("b",8), ("c", 2)] == Just 67
                 ]
test_evalMRef2_fail =
  evalMRef e9 [("a",7), ("b",8)] 
