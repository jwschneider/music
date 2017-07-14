import Data.List
import System.IO
import System.Environment

density = 220500 :: Int

{- simple linspace function that returns a list of num elements between left and right -}
linSpace :: Int -> (Double, Double) -> [Double]
linSpace num (left, right)
	| (num == 1) = [right]
	| otherwise = left : linSpace (num - 1) (left + ((right - left)/(fromIntegral num)), right)


sine :: Double -> Double -> Double -> [Double]
sine freq time amp = map (\t -> amp*sin(2*pi*freq*t)) $ linSpace (round ((fromIntegral density)*time)) (0, time)

triangle :: Double -> Double -> Double -> [Double]
triangle freq time amp = map (\t -> amp*abs(2*(t*freq - (fromIntegral (floor (0.5 + t*freq)))))) $ linSpace (round ((fromIntegral density)*time)) (0, time)

sawtooth :: Double -> Double -> Double -> [Double]
sawtooth freq time amp = map (\t -> amp*2*(t*freq - (fromIntegral (floor (0.5 + t*freq))))) $ linSpace (round ((fromIntegral density)*time)) (0, time)


{- Send the data to the handle -}
sendData :: Handle -> [Double] -> IO()
sendData handle (x:[]) = hPrint handle x
sendData handle (x:xs) = do
	hPrint handle x
	sendData handle xs



main = do
	args <- getArgs
	if (length args /= 4) then putStrLn "Incorrect number of arguements"
		else do
			let typ = args!!0
			let freq = args!!1
			let time = args!!2
			let amp = args!!3
			case typ of
				"sin" -> sendData stdout $ (read time) : (sine (read freq) (read time) (read amp))
				"tri" -> sendData stdout $ (read time) : (triangle (read freq) (read time) (read amp)) 
				"saw" -> sendData stdout $ (read time) : (sawtooth (read freq) (read time) (read amp))
