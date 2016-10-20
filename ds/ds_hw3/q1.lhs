|R| <= |L| and |L'| = |L| - |R|

>	data Q a = T [a] [a] [a]
>		deriving Show

>	len (T l r l') = length l + length r

>	insert e (T l r l') = makeq (T l (e:r) l')

>	remove (T (l:ls) r l') = (l, (makeq (T ls r l')))

>	makeq (T l r []) = T l'' [] l''
>		where l'' = rot l r []
>	makeq (T l r (_:l')) = T l r l'

>	rot [] (r:rs) a  = r:a
>	rot (l:ls) (r:rs) a = l:(rot ls rs (r:a))

>	q1 = insert 1 (insert 2 (insert 3 (T [][][])))
>	(v2,q2) = remove q1
>	(v3, q3) = remove q2
>	(v4, q4) = remove q3
