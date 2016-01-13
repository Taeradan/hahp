module HAHP.Algorithm.Validation where

import           Data.Maybe
import           HAHP.Data

-- * Helper functions

validateAHPTree :: AHPTree -> (AHPTree, [ValidationError])
validateAHPTree ahpTree = (ahpTree, catMaybes . concat $
                          [ validateConsistency ahpTree])
--                          , validateTreeStructure ahpTree])

recursiveValidator :: (AHPTree -> Bool)
                   -> (AHPTree -> ValidationError)
                   -> AHPTree
                   -> [Maybe ValidationError]
recursiveValidator testFnt errorFnt ahpTree =
    case ahpTree of
        AHPTree {} -> currentValidity : childrenValidity
            where currentValidity = if testFnt ahpTree
                                    then Nothing
                                    else Just (errorFnt ahpTree)
                  childrenValidity = concatMap (recursiveValidator testFnt errorFnt) (children ahpTree)
        AHPLeaf {} -> [Nothing]

-- * Consistency

validateConsistency :: AHPTree -> [Maybe ValidationError]
validateConsistency ahpTree = recursiveValidator consistencyTest consistencyError ahpTree

consistencyTest :: AHPTree -> Bool
consistencyTest (AHPTree _ _ consistency _ _ _) = isMatrixConsistent (fromJust consistency) validationConsistencyThreshold

consistencyError :: AHPTree -> ValidationError
consistencyError ahpTree =
    ConsistencyError { ahpTree = ahpTree
                     , consistencyThreshold = validationConsistencyThreshold
                     , consistency = fromJust . consistencyValue $ ahpTree
                     }

validationConsistencyThreshold = 0.1

isMatrixConsistent :: Double ->  Double -> Bool
isMatrixConsistent consistency threshold
    | consistency < threshold = True
    | otherwise = False

-- * Tree Structure


