module Algorithm where

import           Configuration
import           Data.Maybe
import           Numeric.LinearAlgebra.Algorithms
import           Numeric.LinearAlgebra.HMatrix


initAHP :: AHPTree -> (AHPTree, Bool)
initAHP ahpTree = (newAHPTree, isTreeValid)
    where isTreeValid = isAHPTreeValid newAHPTree
          newAHPTree = computeTreePriorityVectors (computeTreeConsistencies ahpTree)

randomIndex :: Double -> Double
randomIndex = randomIndexSaaty

randomIndexSaaty :: Double -> Double
randomIndexSaaty matrixSize = saatyTable !! ((round matrixSize) - 1)
    where saatyTable = [0.00, 0.00, 0.58, 0.90, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49, 1.51, 1.48, 1.56, 1.57, 1.59]

-- |Random Index estimation function taken from :
-- "Consistency in the AHP : A new approach"
-- José Antonio ALONSO and Teresa LAMATA,
-- IJUFKBS 2006
randomIndexCalculated :: Double -> Double
randomIndexCalculated matrixSize = ( 0.00149 * (matrixSize^3))
                        + (- 0.05121) * (matrixSize^2)
                        + (  0.59150  * matrixSize)
                        + (- 0.79124)


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

computeTreePriorityVectors :: AHPTree -> AHPTree
computeTreePriorityVectors ahpTree =
    case ahpTree of
         (AHPTree _ prefMat _ _ _ children) -> ahpTree 
            { childrenPriority = Just $ priorityVector prefMat
            , children = map computeTreePriorityVectors children
            }
         AHPLeaf {} -> ahpTree

priorityVector :: PreferenceMatrix -> PriorityVector
priorityVector prefMat = 
    priorityVectorRefining prefMat prefMat (priorityVectorBase prefMat) 

priorityVectorRefining :: PreferenceMatrix -> PreferenceMatrix -> PriorityVector -> PriorityVector
priorityVectorRefining origPrefMat powPrefMat oldPrioVect = 
    if compareMatrixItems newPrioVect oldPrioVect threshold
        then newPrioVect
        else priorityVectorRefining origPrefMat (powPrefMat <> origPrefMat) newPrioVect
    where newPrioVect = priorityVectorBase powPrefMat
          threshold = 1.11e-16
          matrixSize = fromIntegral $ rows origPrefMat

compareMatrixItems :: (Matrix Double) -> (Matrix Double) -> Double -> Bool
compareMatrixItems matrixA matrixB threshold =
    all (\(x,y) -> abs(x - y) < threshold ) list
        where list = zip (toList $ flatten matrixA) (toList $ flatten matrixB)

priorityVectorBase :: PreferenceMatrix -> PriorityVector
priorityVectorBase prefMat = numerator <> inv denominator
    where numerator = prefMat <> eT
          denominator = e <> prefMat <> eT
          e = (1 >< matrixSize )[1, 1..]
          eT = (matrixSize >< 1 )[1, 1..]
          matrixSize = fromIntegral $ rows prefMat

matrixConsistency :: PreferenceMatrix -> Double
matrixConsistency prefMat = consistencyIndicator / randomIndexValue
    where randomIndexValue = randomIndex matrixSize
          consistencyIndicator = (lambdaMax - matrixSize) / (matrixSize - 1)
          lambdaMax = maxEigenValue prefMat
          matrixSize = fromIntegral $ rows prefMat

maxEigenValue :: PreferenceMatrix -> Double
maxEigenValue prefMat = realPart $ maxElement $ eigenvalues prefMat


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

