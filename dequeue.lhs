Based on Chris Okasaki's paper.

>	data Deq a = Quad [a] [a] [a] [a]
>		deriving Show

SO we now have a quadruple: (L, R, L', R') where L' and R' are the
tails of L and R indicating which portions have been pre-evaluated.

Because of the symmetry of the front and end we want the two to
be roughly the same size. We choose a constant c such that:

	|L| <= c|R| + 1 and |R| <= c|L| + 1
We'll choose c = 3.

So we'll have to track the sizes and if
the size invariant would be violated, we rot:


>	rot1 (n,l,r)
>		| n >= 3		= head l :rot1 (n-3, tl l, drop 3 r)
>		| otherwise		= rot2 (l, drop n r, [])


>	rot2 (l,r,a) 
>		| length l > 0 && length r >= 3	= head l : rot2(tl l, drop 3 r,
>										reverse (take 3 r) ++ a)
>		| otherwise					= l ++ reverse r ++ a

>	len (Quad l r l' r') = length l + length r

>	insertL (e, (Quad l r l' r')) = makedq (e:l, r, tl l', tl r')
>	insertR (e, (Quad l r l' r')) = makedq (l, e:r, tl l', tl r')

>	removeL (Quad l r l'  r')
>		| length l == 0		= (head r, Quad [] [] [] [])
>		| otherwise			=  (head l, makedq (tl l, r, tl (tl l'), tl (tl r')))
>	removeR (Quad l r l'  r')
>		| length r == 0		= (head l, Quad [] [] [] [])
>		| otherwise			= (head r, makedq (l, tl r, tl(tl l'), tl(tl r')))

>	makedq (l,r,l', r') 
>		| length l > (n * length r + 1)		= Quad (take n l) (rot1 (n,r,l)) (take n l) (rot1 (n,r,l))
>		| length r > (n * length l + 1)		= Quad (rot1 (n,l,r)) (take n r) (rot1 (n,l,r))  (take n r) 
>		| otherwise							= Quad l r l' r'
>			where n = (length l + length r) `div` 2

To avoid special cases:

>	tl [] = []
>	tl (x:xs) = xs


The above should be in a where clause, but I was getting parse errors.

			where	l' = take n l
					r' = rot1 (n, r, l)
					m' = rot1 (n, l, r)
					n' = take n r



Note that I used his syntax in this which means you have to present the args
as tuples. Consider curry and uncurry!
