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


-- trick : http://stackoverflow.com/a/7897595
computeTreeConsistency :: AHPTree -> AHPTree
computeTreeConsistency ahpTree =
    case ahpTree of
         (AHPTree _ prefMatrix _ _ _ children) -> ahpTree 
            { consistencyValue = Just $ matrixConsistency prefMatrix
            , children = map computeTreeConsistency children
            }
         AHPLeaf {} -> ahpTree

matrixConsistency :: PreferenceMatrix -> Double
matrixConsistency preferenceMatrix = consistencyIndicator / randomIndexValue
    where randomIndexValue = randomIndex matrixSize
          consistencyIndicator = (maxEigenValue - matrixSize) / (matrixSize - 1)
          maxEigenValue = realPart $ maxElement $ eigenvalues preferenceMatrix
          matrixSize = fromIntegral $ rows preferenceMatrix

isAHPTreeValid :: AHPTree -> Bool
isAHPTreeValid (AHPTree _ _ consistency _ _ children) =
    isMatrixConsistent (fromJust consistency) consistencyThreshold
    && areChildrenValid
        where areChildrenValid = all isAHPTreeValid children

isAHPTreeValid AHPLeaf {} = True

consistencyThreshold = 0.1

isMatrixConsistent :: Double ->  Double -> Bool
isMatrixConsistent consistency threshold
    | consistency < threshold = True
    | otherwise = False

