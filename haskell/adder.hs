import Data.List
import System.IO
import System.Environment

combine :: [Double] -> [Double] -> [Double]
combine (x:[]) ys = [(x + (head ys)) / 2]
combine xs (y:[]) = [((head xs) + y) / 2]
combine (x:xs) (y:ys) = ((x + y) / 2) : combine xs ys


{- Read Doubles from input, sent them to a list -}
getDoubles :: Handle -> IO [Double]
getDoubles handle = do
	done <- hIsEOF handle
	if done then return []
	else do
		first <- hGetLine handle
		rest <- getDoubles handle
		return (read first : rest)

{- Send the data to the handle -}
sendData :: Handle -> [Double] -> IO()
sendData handle (x:[]) = hPrint handle x
sendData handle (x:xs) = do
	hPrint handle x
	sendData handle xs


main = do
	args <- getArgs
	str1 <- openFile (head args) ReadMode
	str2 <- openFile (head (tail args)) ReadMode
	vals1 <- getDoubles str1
	vals2 <- getDoubles str2
	sendData stdout (combine vals1 vals2)
