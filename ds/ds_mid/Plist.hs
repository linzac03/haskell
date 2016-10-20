newtype Plist a = Plist ([a],[a])
	deriving (Show)

append :: Plist a -> Plist a -> Plist a
append (Plist (front,back)) (Plist ([],[])) = Plist (front,back)
append (Plist (front,back)) (Plist ([],ys)) = append (Plist (front,back)) (Plist (reverse ys, []))
append (Plist (front,back)) (Plist ((x:xs),ys)) = append (Plist (front,x:back)) (Plist (xs, ys))

plength :: Plist a -> Int
plength (Plist ([],[])) = 0
plength (Plist ((x:xs),[])) = 1 + plength (Plist (xs,[]))
plength (Plist ([],(y:ys))) = 1 + plength (Plist ([],ys))
plength (Plist ((x:xs),(y:ys))) = 1 + 1 + plength (Plist (xs,ys))

pfold :: (a -> b -> b) -> b -> Plist a -> b
pfold f acc ip@(Plist ([],[])) = acc
pfold f acc ip@(Plist ([],ys)) = pfold f acc (Plist (reverse ys, []))
pfold f acc ip@(Plist ((x:xs),ys)) = pfold f (f x acc) (Plist (xs,ys))

pmap :: (a -> b) -> Plist a -> Plist b
pmap f ip@(Plist (xs,ys)) = Plist (fmap f xs, fmap f ys)

preverse :: Plist a -> Plist a
preverse (Plist (xs,ys)) = Plist (ys, xs)
