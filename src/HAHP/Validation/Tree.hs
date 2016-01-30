module HAHP.Validation.Tree where

import           Control.Parallel.Strategies
--import           Data.List.Unique
import           Data.List                     (group, sort, sortBy)
import           Data.Maybe
import           HAHP.Data
import           HAHP.Validation.Unique
import           Numeric.LinearAlgebra.HMatrix

-- * Helper functions

validateInputAHPTree :: AHPTree -> [TreeError]
validateInputAHPTree ahpTree = validate' ahpTree errorsInputList

validateAHPTree :: AHPTree -> [TreeError]
validateAHPTree ahpTree = validate' ahpTree errorsList

validate' :: AHPTree -> [AHPTree -> Maybe TreeError] -> [TreeError]
validate' ahpTree checks = catMaybes $ concat $ parMap rseq (recursiveValidator ahpTree) checks

errorsInputList :: [AHPTree -> Maybe TreeError]
errorsInputList = [ squareMatrixTest
                  , parentSizeMatchChildrenTest
                  , unitaryDiagTest
                  , nullDivisionTest
                  , positivePreferenceTest
                  , inverseTest
                  , childrenUnicityTest
                  ]

errorsList :: [AHPTree -> Maybe TreeError]
errorsList = [ consistencyTest
             ]

recursiveValidator :: AHPTree
                   -> (AHPTree -> Maybe TreeError)
                   -> [Maybe TreeError]
recursiveValidator ahpTree testFnt =
    case ahpTree of
        AHPTree {} -> currentValidity : childrenValidity
            where currentValidity = testFnt ahpTree
                  --childrenValidity = concatMap (`recursiveValidator` testFnt) (children ahpTree)
                  childrenValidity = concat $ parMap rseq (`recursiveValidator` testFnt) (children ahpTree)
        AHPLeaf {} -> [Nothing]


-- * Tests implementations

-- ** Consistency test

consistencyTest :: AHPTree -> Maybe TreeError
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

childrenUnicityTest :: AHPTree -> Maybe TreeError
childrenUnicityTest ahpTree =
    if null repeatedChildrenNames
       then Nothing
       else Just ChildrenUnicityError { ahpTree = ahpTree
                                      , repeatedChildrenNames = repeatedChildrenNames
                                      }
  where repeatedChildrenNames = repeated . map name . children $ ahpTree

parentSizeMatchChildrenTest :: AHPTree -> Maybe TreeError
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

inverseTest :: AHPTree -> Maybe TreeError
inverseTest ahpTree =
    if all (inverseTest' . preferenceMatrix $ ahpTree) indices
       then Nothing
       else Just InverseError {ahpTree = ahpTree}
    where  indices = [ (i, j)
                     | i <- [1..matrixSize-1]
                     , j <- [1..matrixSize-1]
                     , j > i
                     ]
           matrixSize = fromIntegral . rows . preferenceMatrix $ ahpTree

inverseTest' :: Matrix Double -> (Int, Int) -> Bool
inverseTest' matrix (i, j) = m_ij == (1 / m_ji)
    where
          m_ij :: Double
          m_ij = matrix `atIndex` (i, j)
          m_ji :: Double
          m_ji = matrix `atIndex` (j, i)

nullDivisionTest :: AHPTree -> Maybe TreeError
nullDivisionTest ahpTree =
    if 0 `notElem` matrixvalues
       then Nothing
       else Just NullDivisionError {ahpTree = ahpTree}
  where matrixvalues = concat . toLists . preferenceMatrix $ ahpTree

positivePreferenceTest :: AHPTree -> Maybe TreeError
positivePreferenceTest ahpTree =
    if all (> 0) matrixValues
       then Nothing
       else Just PositivePreferenceError {ahpTree = ahpTree}
  where matrixValues = concat . toLists . preferenceMatrix $ ahpTree

squareMatrixTest :: AHPTree -> Maybe TreeError
squareMatrixTest ahpTree =
    if rows matrix == cols matrix
       then Nothing
       else Just SquareMatrixError { ahpTree = ahpTree
                                   , errorRows = rows matrix
                                   , errorCols = cols matrix
                                   }
  where matrix = preferenceMatrix ahpTree

unitaryDiagTest :: AHPTree -> Maybe TreeError
unitaryDiagTest ahpTree =
    if all (== 1) diagonalValues
       then Nothing
       else Just NotUnitaryDiagError {ahpTree = ahpTree}
  where diagonalValues = toList . takeDiag . preferenceMatrix $ ahpTree

