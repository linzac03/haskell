module Test where

import Stack 
import Queue 

makeStack :: String -> Stack Char
makeStack s = makeStack' s (Stack [])
	where
		makeStack' [] ns = ns
		makeStack' s@(x:xs) ns = makeStack' xs (push x ns)

makeQueue :: String -> Queue Char
makeQueue s = Queue s

isPalindrome :: String -> Bool
isPalindrome s = palindrome (makeQueue s) (makeStack s)
	where
		palindrome queue stack
			| isEmpty (snd $ pop stack) 
				|| isEmptyQueue (snd $ dequeue queue)	= fst (dequeue queue) == fst (pop stack)
			| otherwise									= fst (dequeue queue) == fst (pop stack) 
															&& palindrome (snd $ dequeue queue) (snd $ pop stack)

test_isEmpty = isEmpty (Stack [])
test_isEmptyQueue = isEmptyQueue (Queue [])
