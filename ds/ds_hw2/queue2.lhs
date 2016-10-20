This is from the lecture notes from upenn on functional ds:

A better queue!

The goal is to optimize enqueue and dequeue so that it is mostly constant
time to do these operations. 

>	data Queue a = Queue [a] [a]
>		deriving Show
>	
>	enqueue :: a -> Queue a -> Queue a
>	enqueue e (Queue front back) = Queue front (e:back)

Above is constant time, since we don't need to traverse the list.

>	dequeue :: Queue a -> (a, Queue a)
>	dequeue (Queue (e:front) back) = (e, (Queue front back))
>	dequeue (Queue [] back) = dequeue (Queue (reverse back) [])

Note that for a dequeue, we remove from the front UNLESS it is empty,
in which case we reverse the back, and make it the front. So this
is "expensive", but it is one-time for many dequeues.

So enqueue always tacks the new element onto the front of the "back". This
is O(1).

An element will only be involved in an "expensive" operation once: after
it has been reversed, it will then be available to pluck. When an element
is enqueued, it goes on the back: one cons operation. When it is time for it to
be dequeued, it gets reversed, and the de-cons'd. Reverse itself is usually
implemented as a series of Cons operations (so it represents deferred cons
operations.)

So we can talk of the amortized cost of an enqueue: each enqueue involves one
actual cons and one deferred cons. Since we already "paid" for the cost of
the dequeue, the cost of dequeue is 0.

Now let's look at a set implementation. We want to implement the behavior
member and insert. What data structure should we use? One popular way is
a binary search tree (if this is a balanced binary search tree we will have
a member operation of O(log n) and an insert of O(log n).

>	data Set a = E | T (Set a) a (Set a)
>		deriving Show

>	empty = E

>	member x E = False
>	member x (T a y b)
>		| x < y		= member x a
>		| x > y		= member x b
>		| otherwise	= True

>	insert x E = T E x E
>	insert x t@(T a y b)
>		| x < y		= T (insert x a) y b
>		| x > y		= T a y (insert x b)
>		| otherwise	= t

>	type Set2 a = a -> Bool
>		deriving Show


What if the tree is unbalanced? A long search path (a long path in the tree)
can lead to linear time searches instead of log n.

There are a number of balanced bsts: AVL trees, red-black trees, 0-1-2 trees,
etc. We'll look at one: red black trees.

These maintain balance by ensuring that no path is more than twice as long
as the shorted path. (It's not as good as an AVL tree but cheaper to maintain)

>	data Color = R | B
>		deriving Show

>	data RedBlackSet a = RB_E | RBT Color (RedBlackSet a) a (RedBlackSet a)
>		deriving Show

The empty tree and membership is mostly the same as before (we just
ignore the color, which is not relevant to the search.)

>	rb_empty = RB_E

>	rbmember x RB_E = False
>	rbmember x (RBT _ a y b)
>		| x < y	=	 	rbmember x a
>		| x > y =	 	rbmember x b
>		| otherwise	=	True


But on insertion, we may violate one of the red-black properties. We may find
that we insert and no longer have the same number of black nodes. Or that we
now have a red child of a red parent. These color attributes are used to
maintain the boundaries on path length.

We'll introduce a new auxilliary function ins that creates a red node
with no children subtrees. Now we need to find where this node should reside.
If we are inserting into an empty tree: no problem. But a non-empty tree
requires us to find the right location according to the order of the
nodes. (If the item is already in the tree, do nothing.)

	ins RB_E = RBT R E x E
	ins s@(RBT color a y b)
		| x < y			= (RBT color (ins a) y b)
		| x > y			= (RBT color a y (ins a b))
		| otherwise		= s

This is not so bad, but we may have violated a color invariant. So
we need to rebalance.


	ins RB_E = RBT R E x E
	ins s@(RBT color a y b)
		| x < y			= balance (RBT color (ins a) y b)
		| x > y			= balance (RBT color a y (ins a b))
		| otherwise		= s

There are four kinds of unbalance that can occur.
We may have a black-red-red path in any of 4 ways: black
grandparent, one of two red children, one of 4 red grandchildren.
The patterns that may occur (thinking of directions from the grandparent)
are L,L, L,R, R,L, R,R. So we can capture these unbalances as patterns.

LL: change root to Red, and children to black

>	balance (RBT B (RBT R (RBT R a x b) y c) z d) =
>		RBT R (RBT B a x b) y (RBT B c z d)

LR: change root to Red and children to black

>	balance (RBT B (RBT R a x (RBT R b y c)) z d) =
>		RBT R (RBT B a x b) y (RBT B c z d)

RL: change root to red and children to black

>	balance (RBT B a x (RBT R (RBT R b y c) z d)) =
>		RBT R (RBT B a x b) y (RBT B c z d)

RR: change root to read and children to black

>	balance (RBT B a x (RBT R b y (RBT R c z d))) =
>		RBT R (RBT B a x b) y (RBT B c z d)

>	balance t = t

Then, since the root of the tree must be black, if we are
rebalancing around a root, we force its color to be black no
matter what ins returned.


SO final version:

>	rbinsert x t = makeRootBlack (ins t)
>	  where
>		ins RB_E = RBT R RB_E x RB_E
>		ins s@(RBT color a y b)
>		  | x < y			= balance (RBT color (ins a) y b)
>		  | x > y			= balance (RBT color a y (ins b))
>		  | otherwise		= s
>		makeRootBlack (RBT _ a y b) = RBT B a y b

