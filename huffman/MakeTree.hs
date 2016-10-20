module MakeTree (makeTree) where
import Types


value :: Tree -> Int 
value (Leaf _ n) = n
value (Node n _ _) = n

pair :: Tree -> Tree -> Tree
pair t1 t2 = Node (v1+v2) t1 t2
	where
		v1 = value t1
		v2 = value t2

insTree :: Tree -> [Tree] -> [Tree]
insTree t1 [] = [t1]
insTree t1 t2@(t:ts)
	| (<=) (value t1) (value t) = t1 : t2
	| otherwise 				= t : insTree t1 ts

amalgamate :: [Tree] -> [Tree]
amalgamate (t1:t2:ts) = insTree (pair t1 t2) ts

makeTree :: [(Char,Int)] -> Tree
makeTree = makeCodes . toTreeList
	where
		toTreeList :: [(Char,Int)] -> [Tree]
		toTreeList = map (uncurry Leaf)
		makeCodes :: [Tree] -> Tree
		makeCodes [t] = t
		makeCodes ts = makeCodes (amalgamate ts) 
