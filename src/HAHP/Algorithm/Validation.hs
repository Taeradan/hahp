module HAHP.Algorithm.Validation where

import           Data.Maybe
import           HAHP.Data
import           Numeric.LinearAlgebra.HMatrix

-- * Helper functions

validateInputAHPTree :: AHPTree -> (AHPTree, [ValidationError])
validateInputAHPTree ahpTree = (ahpTree, catMaybes $ concatMap (recursiveValidator ahpTree) errorsInputList)

validateAHPTree :: AHPTree -> (AHPTree, [ValidationError])
validateAHPTree ahpTree = (ahpTree, catMaybes $ concatMap (recursiveValidator ahpTree) errorsList)

errorsInputList :: [ ((AHPTree -> Bool), (AHPTree -> ValidationError)) ]
errorsInputList = [(squareMatrixTest, squareMatrixError)
                  ]
errorsList :: [ ((AHPTree -> Bool), (AHPTree -> ValidationError)) ]
errorsList = [ (consistencyTest, consistencyError)
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
consistencyTest (AHPTree _ _ consistency _ _ _) =
    case consistency of
      Nothing -> False
      Just x -> isMatrixConsistent x validationConsistencyThreshold

consistencyError :: AHPTree -> ValidationError
consistencyError ahpTree =
    case (consistencyValue ahpTree) of
      Nothing -> NotComputedConsistencyError { ahpTree = ahpTree }
      Just x ->  ConsistencyError { ahpTree = ahpTree
                                  , consistencyThreshold = validationConsistencyThreshold
                                  , consistency = x
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
