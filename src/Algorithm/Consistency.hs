module Algorithm.Consistency where

import           Data
import           Numeric.LinearAlgebra.HMatrix

-- trick : http://stackoverflow.com/a/7897595
-- trick2 : https://wiki.haskell.org/Default_values_in_records
computeTreeConsistencies :: AHPTree -> AHPTree
computeTreeConsistencies ahpTree =
    case ahpTree of
        (AHPTree _ prefMat _ _ _ children) -> ahpTree
            { consistencyValue = Just $ matrixConsistency prefMat
            , children = map computeTreeConsistencies children
            }
        AHPLeaf {} -> ahpTree

-- |A matrix with consistency=0 is consistent.
matrixConsistency :: PairwiseMatrix -> Double
matrixConsistency prefMat
    | matrixSize <= 2 = 0
    | otherwise = consistencyIndicator / randomIndexValue
    where randomIndexValue = randomIndex matrixSize
          consistencyIndicator = (lambdaMax - matrixSize) / (matrixSize - 1)
          lambdaMax = maxEigenValue prefMat
          matrixSize = fromIntegral . rows $ prefMat

maxEigenValue :: PairwiseMatrix -> Double
maxEigenValue prefMat = realPart . maxElement . eigenvalues $ prefMat

randomIndex :: Double -> Double
randomIndex size
    | size <= 15 = randomIndexSaaty size
    | otherwise = randomIndexCalculated size

randomIndexSaaty :: Double -> Double
randomIndexSaaty matrixSize = saatyTable !! (round matrixSize - 1)
    where saatyTable = [0.00, 0.00, 0.58, 0.90, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49, 1.51, 1.48, 1.56, 1.57, 1.59]

-- |Random Index estimation function taken from :
-- "Consistency in the AHP : A new approach"
-- José Antonio ALONSO and Teresa LAMATA,
-- IJUFKBS 2006
randomIndexCalculated :: Double -> Double
randomIndexCalculated matrixSize = ( 0.00149   * (matrixSize^3))
                                 + (- 0.05121) * (matrixSize^2)
                                 + (  0.59150  *  matrixSize)
                                 + (- 0.79124)
