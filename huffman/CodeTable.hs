module CodeTable (codeTable) where
import Types

convert :: HCode -> Tree -> Table
convert cd (Leaf c n) = [(c,cd)]
convert cd (Node n t1 t2) = (convert (cd ++ [L]) t1) 
							++ (convert (cd++[R]) t2)

codeTable :: Tree -> Table
codeTable = convert []
