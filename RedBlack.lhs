
>	data Color = R | B
>		deriving Show

>	data RedBlackSet a = RB_E | RBT Color (RedBlackSet a) a (RedBlackSet a)
>		deriving Show

>	balance (RBT B (RBT R (RBT R a x b) y c) z d) =
>		RBT R (RBT B a x b) y (RBT B c z d)
>	balance (RBT B (RBT R a x (RBT R b y c)) z d) =
>		RBT R (RBT B a x b) y (RBT B c z d)
>	balance (RBT B a x (RBT R (RBT R b y c) z d)) =
>		RBT R (RBT B a x b) y (RBT B c z d)
>	balance (RBT B a x (RBT R b y (RBT R c z d))) =
>		RBT R (RBT B a x b) y (RBT B c z d)

>	balance t = t


>	rbinsert x t = makeRootBlack (ins t)
>	  where
>		ins RB_E = RBT R RB_E x RB_E
>		ins s@(RBT color a y b)
>		  | x < y			= balance (RBT color (ins a) y b)
>		  | x > y			= balance (RBT color a y (ins b))
>		  | otherwise		= s
>		makeRootBlack (RBT _ a y b) = RBT B a y b


>	r1 = rbinsert 4 RB_E
>	r2 = rbinsert 5 r1
>	r3 = rbinsert 6 r2
>	r4 = rbinsert 7 r3
>	r5 = rbinsert 8 r4
