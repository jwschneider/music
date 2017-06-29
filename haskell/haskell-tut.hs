import Data.List
import System.IO
import System.Environment


samplingRate = 44100 :: Double
bitDepth = 32700 :: Double

{- Takes in the number of the key on the piano and returns the corresponding
	frequency in hz -}
keyFreq :: Int -> Double
keyFreq n 
	| (n >= 1) && (n <= 88) = (2 ** ((fromIntegral (n - 49)) / 12)) * 440
	| otherwise = 0
{- Takes in a Key number and a Time in seconds, and returns 
	the list of doubles to be interpereted by pacat at 44.1k s16le -}
playNote :: Int -> Double -> [Int]
playNote key time = map (\ y ->  round y::Int) $ map (\ x -> bitDepth*sin(2*pi*x/(keyFreq key))) [0..((samplingRate*time)-1)]

printItems :: [Int] -> Handle -> IO()
printItems (x:[]) handle = hPrint handle x
printItems (x:xs) handle = do
	hPrint handle x
	printItems  xs handle

main = do 
	args <- getArgs
	let key = read (args!!0) :: Int
	printItems  (playNote key 1) stdout
	
