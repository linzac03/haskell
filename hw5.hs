
import Data.Char

data Expr = Lit Integer |
			Add Expr Expr |
			Sub Expr Expr |
			Mul Expr Expr |
			Div Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++ ")"
showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ "-" ++ showExpr e2 ++ ")"

-- 14.4 --
size :: Expr -> Integer
size (Lit n) = 0
size (Add e1 e2) = 1 + size e1 + size e2
size (Sub e1 e2) = 1 + size e1 + size e2

-- 14.5 --
eval' :: Expr -> Integer
eval' (Lit n) = n
eval' (Add e1 e2) = (eval' e1) + (eval' e2)
eval' (Sub e1 e2) = (eval' e1) - (eval' e2)
eval' (Mul e1 e2) = (eval' e1) * (eval' e2)
eval' (Div e1 e2) = (eval' e1) `div` (eval' e2)

showExpr' :: Expr -> String
showExpr' (Lit n) = show n
showExpr' (Add e1 e2) = "(" ++ showExpr' e1 ++ "+" ++ showExpr' e2 ++ ")"
showExpr' (Sub e1 e2) = "(" ++ showExpr' e1 ++ "-" ++ showExpr' e2 ++ ")"
showExpr' (Mul e1 e2) = "(" ++ showExpr' e1 ++ "*" ++ showExpr' e2 ++ ")"
showExpr' (Div e1 e2) = "(" ++ showExpr' e1 ++ "/" ++ showExpr' e2 ++ ")"

size' :: Expr -> Integer
size' (Lit n) = 0
size' (Add e1 e2) = 1 + size' e1 + size' e2
size' (Sub e1 e2) = 1 + size' e1 + size' e2
size' (Mul e1 e2) = 1 + size' e1 + size' e2
size' (Div e1 e2) = 1 + size' e1 + size' e2

-- 14.6 --
data Expr' = Lit' Integer |
			 Op Ops Expr' Expr'
data Ops = Add' | Sub' | Mul' | Div'
	deriving(Eq)

eval'' :: Expr' -> Integer
eval'' (Lit' n) = n
eval'' (Op o e1 e2) 
	| o == Add'	= (eval'' e1) + (eval'' e2)
	| o == Sub'	= (eval'' e1) - (eval'' e2)
	| o == Mul'	= (eval'' e1) * (eval'' e2)
	| o == Div'	= (eval'' e1) `div` (eval'' e2)

showExpr'' :: Expr' -> String
showExpr'' (Lit' n) = show n
showExpr'' (Op o e1 e2) 
	| o == Add'	= "(" ++ showExpr'' e1 ++ "+" ++ showExpr'' e2 ++ ")"
	| o == Sub'	= "(" ++ showExpr'' e1 ++ "-" ++ showExpr'' e2 ++ ")"
	| o == Mul'	= "(" ++ showExpr'' e1 ++ "*" ++ showExpr'' e2 ++ ")"
	| o == Div'	= "(" ++ showExpr'' e1 ++ "/" ++ showExpr'' e2 ++ ")"

size'' :: Expr' -> Integer
size'' (Lit' n) = 0
size'' (Op o e1 e2) 
	| o == Add'	= 1 + size'' e1 + size'' e2
	| o == Sub'	= 1 + size'' e1 + size'' e2
	| o == Mul'	= 1 + size'' e1 + size'' e2
	| o == Div'	= 1 + size'' e1 + size'' e2

-- 14.8 --
data InExpr = Lite Integer |
			  InExpr :+: InExpr |
			  InExpr :-: InExpr

inEval :: InExpr -> Integer
inEval (Lite n) = n
inEval (e1 :+: e2) = (inEval e1) + (inEval e2)
inEval (e1 :-: e2) = (inEval e1) - (inEval e2)

showInExpr :: InExpr -> String
showInExpr (Lite n) = show n
showInExpr (e1 :+: e2) = "(" ++ showInExpr e1 ++ "+" ++ showInExpr e2 ++ ")"
showInExpr (e1 :-: e2) = "(" ++ showInExpr e1 ++ "-" ++ showInExpr e2 ++ ")"

-- 14.10 -- 14.13, 14.15, 14.19, 14.20
data NTree = NilT | Node Integer NTree NTree

elemNTree :: Integer -> NTree -> Bool
elemNTree _ NilT = False
elemNTree n (Node m nt1 nt2) 
	| n /= m 	= elemNTree n nt1 || elemNTree n nt2
	| otherwise = True

testTree = (Node 2 (Node 4 (NilT) (NilT)) (Node 1 (Node 7 (NilT) (NilT)) (Node 8 (Node 0 (NilT) (Node 99 (NilT) (NilT))) (NilT)))) 

-- 14.13 --
collapse, sort :: NTree -> [Integer]

collapse (NilT) = []
collapse (Node n nt1 nt2) = collapse nt1 ++ [n] ++ collapse nt2

sort (NilT) = []
sort nt@(Node n nt1 nt2) = bubsort $ collapse nt
	where 
		bubsort lss@(x:xs) = bub (length lss) lss
			where
				bub _ [] = []
				bub 0 ls = ls
				bub n ls@(x:xs) 
					| xs == []	= [x]
					| x > head xs = bub (n-1) ([head xs] ++ (bub n ([x] ++ tail xs)))
					| x < head xs = bub (n-1) ([x] ++ bub n xs)

-- 14.15 --
--we did this in class i think

-- 14.19 --
--skip
-- 14.20 --
--skip

-- bin2int --
data Bit = One | Zero
	deriving(Eq,Show)

bin2int :: [Bit] -> Int
bin2int [] = 0
bin2int ls@(x:xs)
	| x /= One	= bin2int xs
	| otherwise = 2^((length ls)-1) + bin2int xs

bin2int' :: [Bit] -> Int
bin2int' [] = 0
bin2int' bs = foldr acc 0 (reverse bs)
	where 
		acc b n
			| b /= Zero	= 2*n + 1
			| otherwise = 2*n

int2bit :: Int -> [Bit]
int2bit 0 = [Zero]
int2bit 1 = [One]
int2bit n 
	| n `mod` 2	== 0	= int2bit (n `div` 2) ++ [Zero] 
	| otherwise 		= int2bit (n `div` 2) ++ [One]

-- 
transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]   -- simulates a transmission channel.
channel = id

make8 :: [Bit] -> [Bit] 
make8 bs 
	| length bs < 8 	= make8 $ [Zero] ++ bs
	| otherwise			= bs

encode :: String -> [Bit]
encode s = concat $ fmap (\ ch -> make8 $ int2bit $ ord ch) s

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bs = [take 8 bs] ++ chop8 (drop 8 bs)

decode :: [Bit] -> String
decode bs = fmap (\ b -> chr $ bin2int b) $ chop8 bs

--unfolds0--
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

--int2bit--
{-int2bitun :: Int -> [Bit]
int2bitun 0 = [Zero]
int2bitun 1 = [One]
int2bitun x = int2bitun (t x) : [h x]
		where
			p = (\ n -> n `mod` 2 == 0) 
			h = (\ f -> if p f then Zero else One)
			t = (\ i -> i `div` 2)
-}
--map f--

