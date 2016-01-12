module HAHP.Algorithm.Validation where

import           Data.Maybe
import           HAHP.Data

-- * Helper function

validateAHPTree :: AHPTree -> (AHPTree, [ValidationError])
validateAHPTree ahpTree = (ahpTree, catMaybes . validateConsistency $ ahpTree)

-- * Consistency

validateConsistency :: AHPTree -> [Maybe ValidationError]
validateConsistency ahpTree =
    case ahpTree of
        (AHPTree _ _ consistency _ _ children) -> currentValidity : childrenValidity
            where currentValidity = if test
                                then Nothing
                                else Just (ConsistencyError ahpTree consistencyThreshold localConsistency)
                  test = isMatrixConsistent localConsistency consistencyThreshold
                  localConsistency = fromJust consistency
                  childrenValidity = concatMap validateConsistency children
        AHPLeaf {} -> [Nothing]

consistencyThreshold = 0.1

isMatrixConsistent :: Double ->  Double -> Bool
isMatrixConsistent consistency threshold
    | consistency < threshold = True
    | otherwise = False

-- * Tree Structure

isTreeStructureValid :: AHPTree -> Bool
isTreeStructureValid = null . checkTreeStructure

checkTreeStructure :: AHPTree -> [ValidationError]
checkTreeStructure = checkTreeStructure' []

-- TODO : implement, issue 6
checkTreeStructure' :: [ValidationError] -> AHPTree -> [ValidationError]
checkTreeStructure' prevErrors ahpTree = prevErrors ++ newErrors
    where newErrors = []
