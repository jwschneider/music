import Data.List
import System.IO

samplingRate = 44100 :: Double
bitDepth = 65536 :: Double

{- Takes in the number of the key on the piano and returns the corresponding
	frequency in hz -}
keyFreq :: Int -> Double
keyFreq n 
	| (n >= 1) && (n <= 88) = (2 ** ((fromIntegral (n - 49)) / 12)) * 440
	| otherwise = 0
{- Takes in a Key number and a Time in seconds, and returns 
	the list of doubles to be interpereted by pacat at 44.1k s16le -}
playNote :: Int -> Double -> [Int]
playNote key time = map (\ y -> round y :: Int) $ map (\ x -> bitDepth*sin(2*pi*x/(keyFreq key))) [0..((samplingRate*time)-1)]

printItems :: [Int] -> IO()
printItems (x:[]) = print x
printItems (x:xs) = do
	print x
	printItems xs

main = printItems $ playNote 49 2
