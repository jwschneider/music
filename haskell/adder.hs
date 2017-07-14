import Data.List
import System.IO
import System.Environment

combine :: [[Double]] -> [Double]
combine strs
	| (length strs > 2) = combine ((zipWith (\x y -> (x + y) / 2) (head strs) (head (tail strs))) : (tail (tail strs)))
	| (length strs == 2) = zipWith (\x y -> (x + y) / 2) (head strs) (head (tail strs))
	| (length strs == 1) = head strs

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
sendData handle [] = return ()
sendData handle (x:[]) = hPrint handle x
sendData handle (x:xs) = do
	hPrint handle x
	sendData handle xs


main = do
	args <- getArgs
	handleList <- mapM (\x -> openFile x ReadMode) args
	doubleList <- mapM getDoubles handleList
	sendData stdout $ combine doubleList
