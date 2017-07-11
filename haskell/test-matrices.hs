import Data.Matrix hiding (toList)
import Data.Vector hiding (length, map)

vals = [3, 5, 7, 11, 13, 17, 19] :: [Double]

divDif :: Matrix (Double, Double) -> (Int, Int) -> Matrix (Double, Double)
divDif mat (i, j)
	|((snd (getElem i j mat)) == 1) = mat
	|(j==1) = setElem ((vals!!(i-1)), 1) (i, j) mat
	|otherwise = do
	let mat1 = divDif mat (i-1, j-1)
	let mat2 = divDif mat1 (i, j-1)
	setElem ((((fst (getElem i (j-1) mat2)) - (fst (getElem (i-1) (j-1) mat2)))/(fromIntegral (j-1))), 1) (i, j) mat2

stripDif :: Matrix (Double, Double) -> [Double]
stripDif mat = map (\(i, j) -> i) (toList (getDiag mat))

mat = matrix (length vals) (length vals) (\(i, j) -> (0::Double, 0::Double))

main = print $ stripDif (divDif mat ((length vals), (length vals)))
