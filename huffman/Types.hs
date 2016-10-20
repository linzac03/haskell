module Types (Tree(Leaf,Node),
			  Bit(L,R),
			  HCode,
			  Table ) where

data Tree = Leaf Char Int | Node Int Tree Tree
	deriving (Show)

data Bit = L | R 
	deriving (Eq,Show)

type HCode = [Bit]

type Table = [(Char,HCode)]
