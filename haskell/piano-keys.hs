import Data.List
import System.IO
import System.Environment

density = 1000000 :: Int

{- Takes in the number of the key on the piano and returns the corresponding
	frequency in hz -}
keyFreq :: Int -> Double
keyFreq n 
	| (n >= 1) && (n <= 88) = (2 ** ((fromIntegral (n - 49)) / 12)) * 440
	| otherwise = 0
{- Takes in a Key number and a Time in seconds, and returns a sin wave with freq*time cycles,
	and with the density of the points equal to the density set -} 
playNote :: Int -> Double -> [Double]
playNote key time = map (\ x -> sin(x)) $ linSpace density (0, ((keyFreq key)*time*2*pi)) 


linSpace :: Int -> (Double, Double) -> [Double]
linSpace num (left, right)
	| (num == 1) = [right]
	| otherwise = left : linSpace (num - 1) (left + ((right - left)/(fromIntegral num)), right)

printItems :: [Double] -> Handle -> IO()
printItems (x:[]) handle = hPrint handle x
printItems (x:xs) handle = do
	hPrint handle x
	printItems  xs handle

main = do 
	args <- getArgs
	if length args == 2
		then do
			let key = read (args!!0) :: Int
			let time = read (args!!1) :: Double
			printItems  (time : (playNote key time)) stdout
		else do
			hPutStrLn stderr "Wrong number of arguments"
	
