module Algorithm where

import           Configuration
import           Numeric.LinearAlgebra.Algorithms
import           Numeric.LinearAlgebra.HMatrix


-- |Random Index estimation function taken from :
-- "Consistency in the AHP : A new approach"
-- José Antonio ALONSO and Teresa LAMATA,
-- IJUFKBS 2006
randomIndex :: Double -> Double
randomIndex matrixSize = ( 0.00149 * (matrixSize^3))
                        + (- 0.05121) * (matrixSize^2)
                        + (  0.59150  * matrixSize)
                        + (- 0.79124)

isAHPTreeValid :: AHPTree -> Bool
isAHPTreeValid (AHPTree name prefMatrix prioVector children) =
    isMatrixConsistent prefMatrix consistencyThreshold
    && areChildrenValid
        where areChildrenValid = all isAHPTreeValid children
isAHPTreeValid _ = True


consistencyThreshold = 0.1

isMatrixConsistent :: PreferenceMatrix -> Double -> Bool
isMatrixConsistent preferenceMatrix threshold
    | check < threshold = True
    | otherwise = False
        where check = matrixConsistency preferenceMatrix

matrixConsistency :: PreferenceMatrix -> Double
matrixConsistency preferenceMatrix = consistencyIndicator / randomIndexValue
    where randomIndexValue = randomIndex matrixSize
          consistencyIndicator = (maxEigenValue - matrixSize) / (matrixSize - 1)
          maxEigenValue = realPart $ maxElement $ eigenvalues preferenceMatrix
          matrixSize = fromIntegral $ rows preferenceMatrix
