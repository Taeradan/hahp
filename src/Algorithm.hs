module Algorithm where

import           Configuration
import           Data.Maybe
import           Numeric.LinearAlgebra.Algorithms
import           Numeric.LinearAlgebra.HMatrix


initAHP :: AHPTree -> (AHPTree, Bool)
initAHP ahpTree = (newAHPTree, isTreeValid)
    where isTreeValid = isAHPTreeValid newAHPTree
          newAHPTree = computePriorityVector (computeTreeConsistency ahpTree)

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
-- trick2 : https://wiki.haskell.org/Default_values_in_records
computeTreeConsistency :: AHPTree -> AHPTree
computeTreeConsistency ahpTree =
    case ahpTree of
         (AHPTree _ prefMatrix _ _ _ children) -> ahpTree 
            { consistencyValue = Just $ matrixConsistency prefMatrix
            , children = map computeTreeConsistency children
            }
         AHPLeaf {} -> ahpTree

computePriorityVector :: AHPTree -> AHPTree
computePriorityVector ahpTree =
    case ahpTree of
         (AHPTree _ prefMatrix _ _ _ children) -> ahpTree 
            { childrenPriority = Just $ priorityVectorCalculus prefMatrix
            , children = map computePriorityVector children
            }
         AHPLeaf {} -> ahpTree

priorityVector :: PreferenceMatrix -> PriorityVector
priorityVector prefMat = 
    priorityVectorRecursive prefMat prefMat (priorityVectorCalculus prefMat) 

priorityVectorRecursive :: PreferenceMatrix -> PreferenceMatrix -> PriorityVector -> PriorityVector
priorityVectorRecursive origPrefMat powPrefMat oldPrioVect = 
    if compareMatrixItems newPrioVect oldPrioVect threshold
        then newPrioVect
        else priorityVectorRecursive origPrefMat (powPrefMat <> origPrefMat) newPrioVect
    where newPrioVect = priorityVectorCalculus powPrefMat
          threshold = 1.11e-16
          matrixSize = fromIntegral $ rows origPrefMat

compareMatrixItems :: PreferenceMatrix -> PreferenceMatrix -> Double -> Bool
compareMatrixItems matrixA matrixB threshold =
    all (\(x,y) -> abs(x - y) < threshold ) list
        where list = zip (toList $ flatten matrixA) (toList $ flatten matrixB)

priorityVectorCalculus :: PreferenceMatrix -> PriorityVector
priorityVectorCalculus preferenceMatrix = newPriorityVector
    where newPriorityVector = numerator <> inv denominator
          numerator = preferenceMatrix <> eT
          denominator = e <> preferenceMatrix <> eT
          e = (1 >< matrixSize )[1, 1..]
          eT = (matrixSize >< 1 )[1, 1..]
          matrixSize = fromIntegral $ rows preferenceMatrix

matrixConsistency :: PreferenceMatrix -> Double
matrixConsistency preferenceMatrix = consistencyIndicator / randomIndexValue
    where randomIndexValue = randomIndex matrixSize
          consistencyIndicator = (lambdaMax - matrixSize) / (matrixSize - 1)
          lambdaMax = maxEigenValue preferenceMatrix
          matrixSize = fromIntegral $ rows preferenceMatrix

maxEigenValue :: PreferenceMatrix -> Double
maxEigenValue preferenceMatrix = realPart $ maxElement $ eigenvalues preferenceMatrix


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

