import Test
import System.IO

main :: IO Bool 
main = do 
		l1 <- getLine
		return (isPalindrome l1) 
		  
