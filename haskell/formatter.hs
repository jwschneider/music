import Data.List
import System.IO
import System.Environment
import System.Random
import Data.Matrix hiding (toList)
import Data.Vector hiding (tail, length, map)

{- author: John Schneider -}

samplingRate = 44100 :: Double
bitDepth = 32700 :: Double

{- Takes list of values, a list of probabilaties, and the mark to beat, 
	returns those of the list of values that beat the mark for a smooth sample -}
sampleByRand :: [Double] -> [Double] -> Double -> [Double]
sampleByRand (x:[]) ys mark
	| ((ys!!0) <= mark) = [x]
	| otherwise = []
sampleByRand (x:xs) ys mark
	| ((ys!!0) <= mark) = x : sampleByRand xs (tail ys) mark
	| otherwise = sampleByRand xs (tail ys) mark

{- Scales the values in the array by the bitDepth -}
scale :: [Double] -> [Double]
scale (x:[]) = [x * bitDepth]
scale (x:xs) = x*bitDepth : scale xs

{- Send the data to the handle -}
sendData :: Handle -> [Double] -> IO()
sendData handle (x:[]) = hPrint handle x
sendData handle (x:xs) = do
	hPrint handle x
	sendData handle xs

{- Read Doubles from input, sent them to a list -}
getDoubles :: IO [Double]
getDoubles = do
	done <- isEOF
	if done then return []
	else do
		first <- getLine
		rest <- getDoubles
		return (read first : rest)

{- Returns the table of divided difference of the matrix mat starting with entry (i,j).
	all of the entries are tuples, with (val, ?modified) -}
divDif :: Matrix (Double, Double) -> (Int, Int) -> Matrix (Double, Double)
divDif mat (i, j)
	|((snd (getElem i j mat)) == 1) = mat
	|(j==1) = setElem ((vals!!(i-1)), 1) (i, j) mat
	|otherwise = do
	let mat1 = divDif mat (i-1, j-1)
	let mat2 = divDif mat1 (i, j-1)
	setElem ((((fst (getElem i (j-1) mat2)) - (fst (getElem (i-1) (j-1) mat2)))/(fromIntegral (j-1))), 1) (i, j) mat2

{- Strips the matrix of divided differences down to a list of elements -}
stripDif :: Matrix (Double, Double) -> [Double]
stripDif mat = map (\(i, j) -> i) (toList (getDiag mat))


main = do
	time <- getLine
	let seglen = read time :: Double
	vals <- getDoubles
	if length vals >= (round (samplingRate*seglen))
		then do
			let g = mkStdGen (round seglen)
			let probs = randoms g :: [Double]
			let vals1 = scale $ sampleByRand vals probs (samplingRate*seglen / (fromIntegral (length vals)))
			hPrint stdout seglen
			sendData stdout vals1
		else do
			hPutStrLn stderr "Not enough sample data to construct wave"
				
