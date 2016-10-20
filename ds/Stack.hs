module Stack (Stack(..), isEmpty, push, pop) where

newtype Stack s = Stack [s]
	deriving(Show)

isEmpty :: Stack s -> Bool
isEmpty (Stack []) = True
isEmpty (Stack ls) = False

push :: s -> Stack s -> Stack s
push item (Stack ls) = Stack (item:ls)

pop :: Stack s -> (s, Stack s)
pop (Stack []) = error "You're a jabronie!!! Loser."
pop (Stack (x:xs)) = (x, Stack xs)
