module HAHP.Algorithm.Validation where

import           Control.Parallel.Strategies
import           Data.Maybe
import           HAHP.Data
import           Numeric.LinearAlgebra.HMatrix

-- * Helper functions

validateInputAHPTree :: AHPTree -> (AHPTree, [ValidationError])
validateInputAHPTree ahpTree = validate' ahpTree errorsInputList

validateAHPTree :: AHPTree -> (AHPTree, [ValidationError])
validateAHPTree ahpTree = validate' ahpTree errorsList

validate' :: AHPTree -> [AHPTree -> Maybe ValidationError] -> (AHPTree, [ValidationError])
validate' ahpTree checks = ( ahpTree
                           , catMaybes $ concatMap (recursiveValidator ahpTree) checks
                           --, catMaybes $ concat $ parMap rseq (recursiveValidator ahpTree) checks
                           )

errorsInputList :: [AHPTree -> Maybe ValidationError]
errorsInputList = [ squareMatrixTest
                  , parentSizeMatchChildrenTest
                  , unitaryDiagTest
                  , nullDivisionTest
                  , positivePreferenceTest
                  ]

errorsList :: [AHPTree -> Maybe ValidationError]
errorsList = [ consistencyTest
             ]

recursiveValidator :: AHPTree
                   -> (AHPTree -> Maybe ValidationError)
                   -> [Maybe ValidationError]
recursiveValidator ahpTree testFnt =
    case ahpTree of
        AHPTree {} -> currentValidity : childrenValidity
            where currentValidity = testFnt ahpTree
                  --childrenValidity = concatMap (`recursiveValidator` testFnt) (children ahpTree)
                  childrenValidity = concat $ parMap rseq (`recursiveValidator` testFnt) (children ahpTree)
        AHPLeaf {} -> [Nothing]

-- * Tests implementations

-- ** Consistency test

consistencyTest :: AHPTree -> Maybe ValidationError
consistencyTest ahpTree =
    case consistencyValue ahpTree of
      Nothing -> Just NotComputedConsistencyError { ahpTree = ahpTree }
      Just x -> if isMatrixConsistent x validationConsistencyThreshold
                   then Nothing
                   else Just ConsistencyError { ahpTree = ahpTree
                                              , consistencyThreshold = validationConsistencyThreshold
                                              , consistency = x
                                              }

validationConsistencyThreshold = 0.1

isMatrixConsistent :: Double -> Double -> Bool
isMatrixConsistent consistency threshold
    | consistency < threshold = True
    | otherwise = False

-- ** Tree structure tests

parentSizeMatchChildrenTest :: AHPTree -> Maybe ValidationError
parentSizeMatchChildrenTest ahpTree =
    if parentSize == childrenSize
       then Nothing
       else Just ParentChildrenSizeMismatchError { ahpTree = ahpTree
                                                 , errorParentSize = parentSize
                                                 , errorChildrenSize = childrenSize
                                                 }
  where parentSize = rows . preferenceMatrix $ ahpTree
        childrenSize = length . children $ ahpTree

-- ** Matrix properties tests

nullDivisionTest :: AHPTree -> Maybe ValidationError
nullDivisionTest ahpTree =
    if 0 `notElem` matrixvalues
       then Nothing
       else Just NullDivisionError {ahpTree = ahpTree}
  where matrixvalues = concat . toLists . preferenceMatrix $ ahpTree

positivePreferenceTest :: AHPTree -> Maybe ValidationError
positivePreferenceTest ahpTree =
    if all (> 0) matrixValues
       then Nothing
       else Just PositivePreferenceError {ahpTree = ahpTree}
  where matrixValues = concat . toLists . preferenceMatrix $ ahpTree

squareMatrixTest :: AHPTree -> Maybe ValidationError
squareMatrixTest ahpTree =
    if rows matrix == cols matrix
       then Nothing
       else Just SquareMatrixError { ahpTree = ahpTree
                                   , errorRows = rows matrix
                                   , errorCols = cols matrix
                                   }
  where matrix = preferenceMatrix ahpTree

unitaryDiagTest :: AHPTree -> Maybe ValidationError
unitaryDiagTest ahpTree =
    if all (== 1) diagonalValues
       then Nothing
       else Just NotUnitaryDiagError {ahpTree = ahpTree}
  where diagonalValues = toList . takeDiag . preferenceMatrix $ ahpTree
