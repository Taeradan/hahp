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
errorsInputList = [ (squareMatrixTest, squareMatrixError)
                  , (parentSizeMatchChildrenTest, parentSizeMatchChildrenError)
                  , (unitaryDiagTest, unitaryDiagError)
                  , (nullDivisionTest, nullDivisionError)
                  , (positivePreferenceTest, positivePreferenceError)
--                  , (childrenNamesUnicityTest, childrenNamesUnicityError)
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

-- * Tests implementations

-- ** Consistency test

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

-- ** Tree structure tests

parentSizeMatchChildrenTest :: AHPTree -> Bool
parentSizeMatchChildrenTest ahpTree = parentSize == childrenSize
    where parentSize = rows . preferenceMatrix $ ahpTree
          childrenSize = length . children $ ahpTree

parentSizeMatchChildrenError :: AHPTree -> ValidationError
parentSizeMatchChildrenError ahpTree =
    ParentChildrenSizeMismatchError { ahpTree = ahpTree
                                    , errorParentSize = parentSize
                                    , errorChildrenSize = childrenSize
                                    }
    where parentSize = rows . preferenceMatrix $ ahpTree
          childrenSize = length . children $ ahpTree

-- ** Matrix properties tests

nullDivisionTest :: AHPTree -> Bool
nullDivisionTest ahpTree = all (/= 0) matrixvalues
    where matrixvalues = concat . toLists . preferenceMatrix $ ahpTree

nullDivisionError :: AHPTree -> ValidationError
nullDivisionError ahpTree = NullDivisionError {ahpTree = ahpTree}

positivePreferenceTest :: AHPTree -> Bool
positivePreferenceTest ahpTree = all (> 0) matrixvalues
    where matrixvalues = concat . toLists . preferenceMatrix $ ahpTree

positivePreferenceError :: AHPTree -> ValidationError
positivePreferenceError ahpTree = PositivePreferenceError {ahpTree = ahpTree}
    where matrixvalues = concat . toLists . preferenceMatrix $ ahpTree

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

unitaryDiagTest :: AHPTree -> Bool
unitaryDiagTest ahpTree = all (== 1) (toList . takeDiag $ matrix)
    where matrix = preferenceMatrix ahpTree

unitaryDiagError :: AHPTree -> ValidationError
unitaryDiagError ahpTree = NotUnitaryDiagError {ahpTree = ahpTree}
