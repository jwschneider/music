import Data.Matrix hiding (toList)
{-import Data.Vector hiding (length, map, head, tail)-}
import Data.Vector (toList)

vals1 = [3, 5, 7] :: [Double]

divDif :: Matrix (Double, Double) -> (Int, Int) -> [Double] -> Matrix (Double, Double)
divDif mat (i, j) vals
	|((snd (getElem i j mat)) == 1) = mat
	|(j==1) = setElem (last vals, 1) (i, j) mat
	|otherwise = do
	let mat1 = divDif mat (i-1, j-1) (init vals)
	let mat2 = divDif mat1 (i, j-1) vals
	setElem ((((fst (getElem i (j-1) mat2)) - (fst (getElem (i-1) (j-1) mat2)))/(fromIntegral (j-1))), 1) (i, j) mat2

stripDif :: Matrix (Double, Double) -> [Double]
stripDif mat = map (\(i, j) -> i) (toList (getDiag mat))

polyEval :: [Double] -> [Double] -> Double -> Double -> Double
polyEval xvals (c:[]) p x0 = p + c
polyEval xvals cs p x0 = polyEval (init xvals) (init cs) ((p + (last cs))*(x0 - (last (init xvals)))) x0

mat = matrix (length vals1) (length vals1) (\(i, j) -> (0::Double, 0::Double))

main = do
	let coeffs = stripDif (divDif mat ((length vals1), (length vals1)) vals1)
	print coeffs
	let xvals = [1, 2, 3] :: [Double]
	print xvals
	let x0 = 0
	print $ polyEval xvals coeffs 0 x0
