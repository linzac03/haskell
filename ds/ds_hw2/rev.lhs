>	reverse''' [] = []
>	reverse''' (x:xs) = reverse''' xs ++ [x]

>	reverse' xs = rev [] xs
>	  where
>		rev acc [] = acc
>		rev acc (x:xs) = rev (x:acc) xs

>	reverse'' xs = foldl revOp [] xs
>	  where
>		revOp a b = b : a
