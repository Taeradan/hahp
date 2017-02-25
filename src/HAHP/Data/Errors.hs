module HAHP.Data.Errors where

import           Data.Map                      (Map)
import           GHC.Generics
import           Numeric.LinearAlgebra.HMatrix

import           HAHP.Data.Core

-- * Errors definition

data TreeError = ConsistencyError { ahpTree              :: AHPTree
                                  , consistencyThreshold :: Double
                                  , consistency          :: Double
                                  }
               | ChildrenUnicityError { ahpTree               :: AHPTree
                                      , repeatedChildrenNames :: [String]
                                      }
               | InverseError { ahpTree :: AHPTree }
               | LeavesUnicityError { ahpTree             :: AHPTree
                                    , repeatedLeavesNames :: [String]
                                    }
               | NotComputedConsistencyError { ahpTree :: AHPTree}
               | NotUnitaryDiagError { ahpTree :: AHPTree }
               | NullDivisionError { ahpTree :: AHPTree}
               | ParentChildrenSizeMismatchError {ahpTree            :: AHPTree
                                                 , errorParentSize   :: Int
                                                 , errorChildrenSize :: Int
                                                 }
               | PositivePreferenceError { ahpTree :: AHPTree
                                         }
               | SquareMatrixError { ahpTree   :: AHPTree
                                   , errorRows :: Int
                                   , errorCols :: Int
                                   }
               deriving (Show)

data AlternativesError = NoAlternativesError {}
                       | AlternativesUnicityError {repeatedAlternativesNames :: [String]}
                       | IndicatorsValuesExistenceError { indValuesErrors :: [(Alternative, String)] }
                    deriving (Show)

