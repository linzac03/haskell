module Queue (Queue(..), isEmptyQueue, enqueue, dequeue) where

newtype Queue t = Queue [t]
	deriving(Show)

isEmptyQueue :: Queue t -> Bool
isEmptyQueue (Queue []) = True
isEmptyQueue (Queue t) = False

test_isEmpty1 = isEmptyQueue (Queue [1,2,3])
test_isEmpty2 = isEmptyQueue (Queue [])
  
enqueue :: t -> Queue t -> Queue t
enqueue item (Queue ls) = Queue (ls ++ [item])

test_eq = enqueue 2.5 (Queue [1.3,2.4,5.5])

dequeue :: Queue t -> (t, Queue t)
dequeue (Queue []) = error "Too many burgers!!!"
dequeue (Queue ls@(x:xs)) = (x, Queue xs)  

test_dq = dequeue (Queue [])
test_dq2 = dequeue (Queue ['a','b','c'])



