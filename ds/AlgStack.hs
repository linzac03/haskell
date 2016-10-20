module AlgStack where

data AlgStack s = Empty | Item s (AlgStack s)
	deriving(Show)

stack = Item 2 (Item 3 (Item 3 (Empty)))

isEmpty :: AlgStack s -> Bool
isEmpty Empty = True
isEmpty _ = False

pop :: AlgStack s -> (s, AlgStack s)
pop Empty = error "No"
pop (Item a q) = (a, q)

push :: s -> AlgStack s -> AlgStack s
push s Empty = Item s (Empty)
push s q 	 = Item s (q)
