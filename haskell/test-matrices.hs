import Data.Matrix

vals = [3, 5, 7] :: [Double]

divDif :: Matrix (Double, Double) -> (Int, Int) -> Matrix (Double, Double)
divDif mat (i, j)
	|((snd (getElem i j mat)) == 1) = mat
	|(j==1) = setElem ((vals!!(i-1)), 1) (i, j) mat
	|otherwise = setElem ((((fst(getElem i (j-1) (divDif mat (i, j-1)))) - (fst(getElem (i-1) (j-1) (divDif mat (i-1, j-1)))))/(fromIntegral (j-1))), 1) (i, j) (divDif mat (i, j-1))

mat = matrix 3 3 (\(i, j) -> (0::Double, 0::Double))

main = print $ divDif mat (3, 3)
