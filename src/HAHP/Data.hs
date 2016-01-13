module HAHP.Data where

import           Data.Map                      (Map)
import           Numeric.LinearAlgebra.HMatrix

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
             deriving (Show)

type IndicatorName = String

type PairwiseMatrix = Matrix Double

type PriorityVector = Matrix Double

data Alternative = Alternative { altName   :: String
                               , indValues :: IndicatorValues
                               }
                 deriving (Show)

type IndicatorValues = Map IndicatorName Double

data ValidationError = ConsistencyError { ahpTree              :: AHPTree
                                        , consistencyThreshold :: Double
                                        , consistency          :: Double
                                        }
                     | NotComputedConsistencyError { ahpTree :: AHPTree}
                     | NotUnitaryDiagError { ahpTree :: AHPTree }
                     | ParentChildrenSizeMismatchError {ahpTree            :: AHPTree
                                                       , errorParentSize   :: Int
                                                       , errorChildrenSize :: Int
                                                       }
                     | SquareMatrixError { ahpTree   :: AHPTree
                                         , errorRows :: Int
                                         , errorCols :: Int
                                         }
                    deriving (Show)
