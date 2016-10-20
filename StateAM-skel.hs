-- file: StateAM-skel.hs
-- Haskell Winter Quarter
-- 03.03.2015
-- TAKE HOME FINAL EXAM.
-- YOU MUST DO YOUR OWN INDIVIDUAL WORK. NO COLLABORATION.

-- Complete the missing code for the evaluators sketched out in this program
-- skeleton. (Marked by ***FILL IN CODE***)
-- DO NOT try to replace these evaluators with entirely different code. Just
-- fill in the code that is missing in this skeleton.
-- I want to see how well you can read, understand, and modify code at this
-- level of abstraction.

-- Use cnc_submit to turn in your exam no later than
-- Tuesday the 10th of March at 5:00pm.
-- Make sure your code passes the testing below!

-- Summary
-- 0. The State Applicative is used to build a simple evaluator
--    expressions with variables that can be both referenced and assigned.

-- 1. A home-grown MonadBind class is defined.

-- 2. The State Monad based on the home-grown MonadBind is used to build
--    the simple evaluator of expressions with variables and assignment.

-- 3. The State Monad based on the Prelude Monad is used to build
--    the simple evaluator of expressions with variables and assignment.


-- Contents
-- 1. Expressions with Assignment
-- 2. The State functor
-- 3. Type Class Definitions
-- 4. Simple Evaluators using State a = Env -> (Env, a)
-- (a) Raw evaluator without the State Applicative or State Monad
-- (b) State evaluator using home-grown Applicative class
-- (c) State Monad evaluator using home-grown MonadBind class
-- (d) State Monad evaluator using the builtin Monad class
-- 5. Testing


------------------------------------------------------------------------------
-- 1. Expressions with Assignment
------------------------------------------------------------------------------

-- The evaluator now assumes abstract expressions have write as well as read
-- variables and only binary Int operators.

-- The expression language has the somewhat quirky (but not uncommon)
-- characteristic that assignments can be used just like any other expression,
-- but they have side effects. The value of an assignment is the value of the
-- right-hand-side expression, along with the side effect on the
-- state, of course.

-- For example the expression  3 + (Asn "a" 5)  in the empty environment []
--     returns the pair (env, val) = ([("a",5)], 8)


-- Abstract Syntax
data ExprAsn = Val Int
             | Var String
             | BinOp (Int -> Int -> Int) ExprAsn ExprAsn
             | Asn String ExprAsn

-- Environment mapping variables to values
type Env = [(String, Int)]

-- A simple environment lookup with abrupt termination on lookup failure
fetch :: String -> Env -> Int
fetch x env = head' [b | (a,b) <- env, x == a ]
	where 
		head' (x:xs) = x
		head' [] = error "Variable does not exist in environment"

-- A simple environment update
store :: String -> Int -> Env -> Env
store var val env = (var, val) : env


------------------------------------------------------------------------------
-- 2. The State functor
------------------------------------------------------------------------------

-- We need the newtype so the functor can be single-parameter as required
-- As a composition:  State a = ((->) Env ) . ((,) Env)
newtype State a = State (Env -> (Env, a))

instance Functor State where
  fmap f (State s) = State $ 
	(\env -> let (ns,a) = s env
			 in (ns, f a))


-- The State functor lookup that we actually need for the Applicative functor
fetchS :: String -> State Int
fetchS var = State (\env -> (env, fetch var env))

-- The State functor update that we actually need for the Applicative functor
storeS :: String -> State Int -> State Int
storeS var (State s) = State ( \env -> let (env', val) = s env
                                       in (store var val env', val) )

-- The general apply function for the State functor (more commonly runST)
applyS :: State Int -> Env -> (Env, Int)
applyS (State s) env = s env


------------------------------------------------------------------------------
-- 3. Type Class Definitions
------------------------------------------------------------------------------

-- The Applicative Class
infixl 4 <*>
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- The MonadBind class
infixl 1 >>>=
class Applicative f => MonadBind f where
  (>>>=) :: f a -> (a -> f b) -> f b
--  returnmb :: a -> f a          -- (>>=) is already taken



------------------------------------------------------------------------------
-- 4. Simple Evaluators using State a = Env -> (Env, a)
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- (a) The raw evaluator without the State Applicative or State Monad
------------------------------------------------------------------------------

evalSRef :: ExprAsn -> Env -> (Env, Int)
evalSRef (Val x) env     =  (env, x) --fill in
evalSRef (Var var) env   =  (env, fetch var env) --fill in
evalSRef (BinOp op e1 e2) env = (env, op (snd $ evalSRef e1 env) (snd $ evalSRef e2 env))
                               
                              
evalSRef (Asn var e) env = ((var, snd ee):env, snd ee)
	where
		ee = evalSRef e env--fill in 
                         


------------------------------------------------------------------------------
-- (b) State evaluator using home-grown Applicative class
------------------------------------------------------------------------------

-- Note the depth-first left-to-right threading of evaluation that
-- the State application operator establishes
instance Applicative State where
  pure g = State (\env -> (env, g))
  State eg <*> State ex = State(\env ->
							let (env', eg') = eg env;
								(env'', ex') = ex env';
							in (env'', eg' ex'))
                         
                        

-- The applicative State evaluator
evalS :: ExprAsn -> State Int
evalS (Val x)          = pure x
evalS (Var var)        = fetchS var 
evalS (BinOp op e1 e2) = pure op <*> evalS e1 <*> evalS e2
evalS (Asn var e)      = storeS var (evalS e) 


-- We need to be able to run the code constructed by EvalS in the state functor
evalSRun :: ExprAsn -> Env -> (Env, Int)
evalSRun expr env = applyS (evalS expr) env

--evalSRun expr env = let State s = evalSM expr
--                    in s env
-- Why didn't we need to do these apply and run with the simple evaluator
-- using only the ((->) Env) arrow functor? Because in our case here we have
-- buried the sequence of actions behind our defined State constructor. That
-- wasn't the case for the simple ((->) Env) evaluator which consists only
-- of built-in type constructors.


------------------------------------------------------------------------------
-- (c) State Monad evaluator using home-grown MonadBind class
------------------------------------------------------------------------------

-- State as a home-grown MonadBind functor
--  (>>>=) :: f a -> (a -> f b) -> f b          -- (>>=) is already taken
instance MonadBind State where
	State s >>>= f    = State $ \env ->
						let (env', val) = s env
						in ap (f val) env'
		where
			ap (State s) e = s e
								


-- The MonadBind State evaluator.
evalSMB :: ExprAsn -> State Int
evalSMB (Val x)          = State $ evalSRun (Val x) --fill in
evalSMB (Var var)        = fetchS var --fill in
evalSMB (BinOp op e1 e2) = evalSMB e1 >>>= (\x ->
						   evalSMB e2 >>>= (\y ->
						   State (\env -> (env, (op x y))))) --fill in USE >>>=



evalSMB (Asn var e)      = evalSMB e >>>= (\ev ->
						   State (\env -> ((var,ev):env, ev)))


-- We need to be able to run the constructed evaluator
evalSMBRun :: ExprAsn -> Env -> (Env, Int)
evalSMBRun expr env = applyS (evalSMB expr) env


------------------------------------------------------------------------------
-- (d) State Monad evaluator using the builtin Monad class
------------------------------------------------------------------------------

-- State as the build-in MonadBind functor
instance Monad State where
  State s >>= f    = State $ \env ->
					 let (env', val) = s env
					 in ap (f val) env'
    where
	  ap (State s) e = s e
                   
  return g = State (\env -> (env,g))

-- The Monad State evaluator
evalSM :: ExprAsn -> State Int
evalSM (Val x)          = return (x) -- fill in
evalSM (Var var)        = fetchS var -- fill in
evalSM (BinOp op e1 e2) = do
						  x <- evalSM e1
						  y <- evalSM e2
						  return (op x y)-- Use >>= or monad "do" ***
                          
                         
evalSM (Asn var e)		= do storeS var $ evalSM e
						  

-- We need to be able to run the constructed evaluator
evalSMRun :: ExprAsn -> Env -> (Env, Int)
evalSMRun expr env = applyS (evalSM expr) env


------------------------------------------------------------------------------
-- 5. Testing
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

-- For assignment tests
e10 = BinOp (+) (BinOp (*) (Asn "a" (Val 6)) (Var "b"))
                (BinOp (+) (Var "a") (BinOp (*) (Asn "b" (Val 3)) (Var "b")))

test_evalSMB = [ evalSMBRun e1 [] == evalSRef e1 [] -- should do em all this way
               ,evalSMBRun e2 [] == evalSRef e2 []
               ,evalSMBRun e3 [] == evalSRef e3 []
               ,evalSMBRun e4 [] == evalSRef e4 []
               ,evalSMBRun e5 [] == evalSRef e5 []
               ,evalSMBRun e6 [("a",5)] == ([("a",5)], 5)
               ,evalSMBRun e7 [("a",5), ("b",8)] == ([("a",5), ("b",8)], 13)
               ,evalSMBRun e8 [("a",7), ("b",2)] == ([("a",7), ("b",2)], 15)
               ,evalSMBRun e9 [("a",7), ("b",8), ("c", 2)]
                         == ([("a",7), ("b",8), ("c", 2)], 67)
               ,evalSMBRun e10 [("a",7), ("b",2)] 
                   == ([("b", 3), ("a",6), ("a", 7), ("b",2)], 27)
               ]

test_evalSM = [ evalSMRun e1 [] == evalSRef e1 [] -- should do em all this way
               ,evalSMRun e2 [] == evalSRef e2 []
               ,evalSMRun e3 [] == evalSRef e3 []
               ,evalSMRun e4 [] == evalSRef e4 []
               ,evalSMRun e5 [] == evalSRef e5 []
               ,evalSMRun e6 [("a",5)] == ([("a",5)], 5)
               ,evalSMRun e7 [("a",5), ("b",8)] == ([("a",5), ("b",8)], 13)
               ,evalSMRun e8 [("a",7), ("b",2)] == ([("a",7), ("b",2)], 15)
               ,evalSMRun e9 [("a",7), ("b",8), ("c", 2)]
                         == ([("a",7), ("b",8), ("c", 2)], 67)
               ,evalSMRun e10 [("a",7), ("b",2)] 
                   == ([("b", 3), ("a",6), ("a", 7), ("b",2)], 27)
               ]


test_evalS = [ evalSRun e1 [] == ([], 3)
              ,evalSRun e2 [] == ([], 7)
              ,evalSRun e3 [] == ([], 25)
              ,evalSRun e4 [] == ([], 39)
              ,evalSRun e5 [] == ([], 5)
              ,evalSRun e6 [("a",5)] == ([("a",5)], 5)
              ,evalSRun e7 [("a",5), ("b",8)] == ([("a",5), ("b",8)], 13)
              ,evalSRun e8 [("a",7), ("b",2)] == ([("a",7), ("b",2)], 15)
              ,evalSRun e9 [("a",7), ("b",8), ("c", 2)]
                          == ([("a",7), ("b",8), ("c", 2)], 67)
              ,evalSRun e10 [("a",7), ("b",2)] 
                          == ([("b", 3), ("a",6), ("a", 7), ("b",2)], 27)
              ]

-- Add more assignment tests

test_evalS_fail =
  evalSRun e9 [("a",7), ("b",8)]

test_evalSRef = [ evalSRef e1 [] == ([], 3)
                 ,evalSRef e2 [] == ([], 7)
                 ,evalSRef e3 [] == ([], 25)
                 ,evalSRef e4 [] == ([], 39)
                 ,evalSRef e5 [] == ([], 5)
                 ,evalSRef e6 [("a",5)] == ([("a",5)], 5)
                 ,evalSRef e7 [("a",5), ("b",8)] == ([("a",5), ("b",8)], 13)
                 ,evalSRef e8 [("a",7), ("b",2)] == ([("a",7), ("b",2)], 15)
                 ,evalSRef e9 [("a",7), ("b",8), ("c", 2)]
                          == ([("a",7), ("b",8), ("c", 2)], 67)
                 ,evalSRun e10 [("a",7), ("b",2)]
                           == ([("b", 3), ("a",6), ("a", 7), ("b",2)], 27)
                 ]

-- Add assignment tests

test_evalSRef_fail =
  evalSRef e9 [("a",7), ("b",8)]

test_all = and test_evalSRef &&
           and test_evalS &&
           and test_evalSMB &&
           and test_evalSM

