Invariant |R| <= |L|


>	data Q a = P [a] [a]
>		deriving Show

>	len (P l r) = length l + length r

>	insert e (P l r) = makeq (P l (e:r))

>	remove (P (l:ls) r) = (l, (makeq (P ls r)))

>	makeq (P l r)
>		| length r <= length l		= P l r
>		| length r == length l + 1	= P (rot l r []) []


>	rot [] (r:rs) a =  r:a
>	rot (l:ls) (r:rs) a = l:rot ls rs (r:a)

>	append q1 (P [] []) = q1
>	append q1@(P l1 r1) q2@(P l2 r2) = append (insert (fst rq) q1) (snd rq)
>		where
>			rq = remove q2 

>	foldp f (P [] []) acc = acc
>	foldp f q acc = foldp f (snd rq) (f acc $ fst rq)
>		where
>			rq = remove q

>	mapp f q = mapp' f q (P [] [])
>		where
>			mapp' f (P [] []) acc = acc
>			mapp' f q acc = mapp' f (snd rq) (insert (f $ fst rq) acc)
>				where
>					rq = remove q	

> 	reversep (P f b) = makeq (P (reverse b) f)

>	q1 = P [1,2,3,4] [7,6,5]
>	(v2,q2) = remove q1
>	(v3, q3) = remove q2
>	(v4, q4) = remove q3

Rewriting:

remove (P [1,2,3,4] [7,6,5] ==>
(1, (makeq (P [2,3,4] [7,6,5]))) ==>
(1, P [2,3,4] [7,6,5]) <---------------- Now the lists are equal

remove (P [2,3,4] [7,6,5]) ==>
(2, (makeq (P [3,4] [7,6,5]))) ==>
(2, (P (rot [3,4] [7,6,5] []) [])) ==>   
(2, (P (3:(rot [4] [6,5] [7])) [])) ==>	tail is suspended: tail of L is rot ...
(2, (P (3:4:(rot [] [5] [6,7])) [])) ==>
(2, (P (3:4:5:6:7:[]) []))

In the sequence of remove: we lop off the head of L, then rot lops off
the head of L'. We also lop off the head of the R and move it to the accumulator. So we remove 2 elements from L for each from R. 

Invariant: |R| <= |L|
Each time we do a remove, we lop off one value from L.
If |R| is still <= |L'|, we are done (the invariant still holds)
If |R| is now |L'| + 1 we call rot. 
We'll call rot L' R' a , lopping off a value from L and R each time.
We started out with a total size of the list of 2n, and
we will make n recursive calls to fully evaluate the L, L', L'', ...
IE to evaluate a list of size 2n, we make at most 2n/2 recursive calls.

