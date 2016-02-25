module HAHP.Data where

import           Data.Map                      (Map)
import           GHC.Generics
import           Numeric.LinearAlgebra.HMatrix

-- * Data set macro type

type AHPDataSet = (AHPTree, [Alternative])

data GeneratorParameters = GeneratorParameters { randomSize       :: Bool
                                               , maxTreeLevels    :: Int
                                               , maxLevelChildren :: Int
                                               , maxAlternatives  :: Int
                                               }

-- * AHP tree definition

data AHPTree = AHPTree { name                 :: String
                       , preferenceMatrix     :: PairwiseMatrix
                       , consistencyValue     :: Maybe Double
                       , childrenPriority     :: Maybe PriorityVector
                       , alternativesPriority :: Maybe PriorityVector
                       , children             :: [AHPTree]
                       }
             | AHPLeaf { name                  :: IndicatorName
                        , maximize             :: Bool
                        , alternativesPriority :: Maybe PriorityVector
                        }
             deriving (Generic, Show)

type IndicatorName = String

type PairwiseMatrix = Matrix Double

type PriorityVector = Matrix Double

-- * Alternatives definition

data Alternative = Alternative { altName   :: String
                               , indValues :: IndicatorValues
                               }
                 deriving (Generic, Show)

type IndicatorValues = Map IndicatorName Double

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

-- * Data retrieve functions

getTreeLeaves :: AHPTree   -- ^ Input tree
              -> [AHPTree] -- ^ List of the leaves
getTreeLeaves ahpTree =
    case ahpTree of
      AHPTree {} -> concatMap getTreeLeaves (children ahpTree)
      AHPLeaf {} -> [ahpTree]
