Trees again ...

>	data Tree a = Leaf a | Branch (Tree a) (Tree a)

>	instance Eq a => Eq (Tree a) where
>		Leaf a1 == Leaf a2				= a1 == a2
>		Branch l1 r1 == Branch l2 r2	= l1 == l2 && r1 == r2
>		_ == _							= False
