module HAHP.Algorithm.Validation where

import           Data.Maybe
import           HAHP.Data
import           Numeric.LinearAlgebra.HMatrix

-- * Helper functions

validateAHPTree :: AHPTree -> (AHPTree, [ValidationError])
validateAHPTree ahpTree = (ahpTree, catMaybes $ concatMap (recursiveValidator ahpTree) errorsList)

errorsList :: [ ((AHPTree -> Bool), (AHPTree -> ValidationError)) ]
errorsList = [ (consistencyTest, consistencyError)
             , (squareMatrixTest, squareMatrixError)
             ]


recursiveValidator :: AHPTree
                   -> ((AHPTree -> Bool), (AHPTree -> ValidationError))
                   -> [Maybe ValidationError]
recursiveValidator ahpTree (testFnt, errorFnt) =
    case ahpTree of
        AHPTree {} -> currentValidity : childrenValidity
            where currentValidity = if testFnt ahpTree
                                    then Nothing
                                    else Just (errorFnt ahpTree)
                  childrenValidity = concatMap (\ x -> recursiveValidator x (testFnt, errorFnt)) (children ahpTree)
        AHPLeaf {} -> [Nothing]

-- * Consistency

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

squareMatrixTest :: AHPTree -> Bool
squareMatrixTest ahpTree = rows matrix == cols matrix
    where matrix = preferenceMatrix ahpTree

squareMatrixError :: AHPTree -> ValidationError
squareMatrixError ahpTree =
    SquareMatrixError { ahpTree = ahpTree
                      , errorRows = rows matrix
                      , errorCols = cols matrix
                      }
        where matrix = preferenceMatrix ahpTree
