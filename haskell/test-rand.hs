import System.Random
import System.IO
import Control.Monad

getDoubleList :: IO [Double]
getDoubleList = do
	done <- isEOF	
	if done then return []
		else do
		input <- getLine
		rest <- getDoubleList
		return (read input : rest)

main = do
	a <- getDoubleList
	print a

