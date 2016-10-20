module AlgQueue where

data AlgQueue a = Empty | Item a (AlgQueue a) 
	deriving(Show)

queue = Item 2 (Item 3 (Item 4 (Empty)))
 
isEmptyQueue :: AlgQueue t -> Bool
isEmptyQueue Empty = True
isEmptyQueue _ = False

dequeue :: AlgQueue t -> (t, AlgQueue t)
dequeue Empty = error "Nothing to dequeue"
dequeue (Item a q) = (a, q)
 
enqueue :: t -> AlgQueue t -> AlgQueue t
enqueue new Empty = Item new (Empty)
enqueue new q = Item (fst $ dequeue q) (enqueue new $ snd $ dequeue q) 
