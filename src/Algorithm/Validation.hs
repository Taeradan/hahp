module Algorithm.Validation where

import           Configuration
import           Data.Maybe

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
