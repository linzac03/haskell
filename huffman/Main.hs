module Main (main, codeMessage, decodeMessage, codes, codeTable) where
import Types (Tree(Leaf,Node), Bit(L,R), HCode, Table)
import Coding (codeMessage, decodeMessage)
import MakeCode (codes, codeTable)

main = print decoded

message :: String
message = "there are green hills here"

treeEx :: Tree
treeEx = codes "there is a green hill"

tableEx :: Table
tableEx = codeTable (codes "there is a green hill")

coded :: HCode
coded = codeMessage tableEx message

decoded :: String
decoded = decodeMessage treeEx coded
