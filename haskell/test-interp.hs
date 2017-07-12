import Data.List
import System.IO
import System.Environment
import System.Random
import Data.Matrix hiding (toList)
import Data.Vector (toList)


{- simple linspace function that returns a list of num elements between left and right -}
linSpace :: Int -> (Double, Double) -> [Double]
linSpace num (left, right)
	| (num == 1) = [right]
	| otherwise = left : linSpace (num - 1) (left + ((right - left)/(fromIntegral num)), right)


{- Returns the table of divided difference of the matrix mat starting with entry (i,j).
	all of the entries are tuples, with (val, ?modified) -}
divDif :: Matrix (Double, Double) -> (Int, Int) -> [Double] -> Matrix (Double, Double)
divDif mat (i, j) vals
	|((snd (getElem i j mat)) == 1) = mat
	|(j==1) = setElem (last vals, 1) (i, j) mat
	|otherwise = do
	let mat1 = divDif mat (i-1, j-1) (init vals)
	let mat2 = divDif mat1 (i, j-1) vals
	setElem ((((fst (getElem i (j-1) mat2)) - (fst (getElem (i-1) (j-1) mat2)))/(fromIntegral (j-1))), 1) (i, j) mat2

{- Strips the matrix of divided differences down to a list of elements -}
stripDif :: Matrix (Double, Double) -> [Double]
stripDif mat = map (\(i, j) -> i) (toList (getDiag mat))

{- Evaluates the polynomial with xvals and coeffs at the point x0 -}
polyEval :: [Double] -> [Double] -> Double -> Double -> Double
polyEval xvals (c:[]) p x0 = p + c
polyEval xvals cs p x0 = polyEval (init xvals) (init cs) ((p + (last cs))*(x0 - (last (init xvals)))) x0



{- Interpolates the polynomial with xvals and coeffs and the points given, returns a 
	list of these points -}
evalChunk :: [Double] -> [Double] -> [Double] -> [Double]
evalChunk (x:[]) xvals coeffs = [polyEval xvals coeffs 0 x]
evalChunk (x:xs) xvals coeffs = (polyEval xvals coeffs 0 x) : (evalChunk xs xvals coeffs)



degree = 100 :: Int

{- Takes an list of values to be sampled, and returns a list of the sampled values -}
sample :: [Double] -> [Double] -> Int -> [Double] -> [Double]
sample [] [] ct out = out
sample buf vals ct out
	| ((length buf) < degree) = sample (buf ++ [head vals]) (tail vals) ct out
	| ((length buf) == degree) = do
	let dD = matrix degree degree (\(i, j) -> (0::Double, 0::Double))
	let coeffs = stripDif $ divDif dD (degree, degree) buf
	if (ct == 10) then out ++ (sample [] vals 1 (evalChunk (linSpace 45 (1, (fromIntegral degree))) [1..(fromIntegral degree)] coeffs))
		else out ++ (sample [] vals (ct + 1) (evalChunk (linSpace 44 (1, (fromIntegral degree))) [1..(fromIntegral degree)] coeffs))

{- Send the data to the handle -}
sendData :: Handle -> [Double] -> IO()
sendData handle (x:[]) = hPrint handle x
sendData handle (x:xs) = do
	hPrint handle x
	sendData handle xs

main = do
	let xs = linSpace 100 (0, 2*pi)
	let wav = map (\x -> sin(x)) xs
	let dD = matrix degree degree (\(i, j) -> (0::Double, 0::Double))
	let coeffs = stripDif $ divDif dD (100, 100) wav
	sendData stdout coeffs	
	
