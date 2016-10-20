data Set a = Empty | Elem a (Set a)
	deriving (Show)

member :: Eq a => a -> Set a -> Bool
member a Empty = False
member a (Elem b (rest)) = if a == b then True else member a rest

insert :: Eq a => a -> Set a -> Set a
insert a Empty = Elem a (Empty)
insert a set   = if member a set then set else Elem a (set)

remove :: Eq a => a -> Set a -> Set a
remove a Empty = Empty
remove a set = if member a set then remove' a set else set
	where
		remove' a (Elem b (rest)) = if a == b then rest else Elem b (remove' a rest)
