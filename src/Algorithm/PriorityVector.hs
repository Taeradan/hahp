module Algorithm.PriorityVector where

import           Configuration
import           Numeric.LinearAlgebra.HMatrix

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

compareMatrixItems :: Matrix Double -> Matrix Double -> Double -> Bool
compareMatrixItems matrixA matrixB threshold =
    all (\(x,y) -> abs(x - y) < threshold ) list
        where list = zip (toList . flatten $ matrixA) (toList . flatten $ matrixB)

priorityVectorBase :: PreferenceMatrix -> PriorityVector
priorityVectorBase prefMat = numerator <> inv denominator
    where numerator = prefMat <> eT
          denominator = e <> prefMat <> eT
          e = (1 >< matrixSize )[1, 1..]
          eT = (matrixSize >< 1 )[1, 1..]
          matrixSize = fromIntegral . rows $ prefMat
