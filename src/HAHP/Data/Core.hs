module HAHP.Data.Core where

import           Data.Map                      (Map)
import           GHC.Generics
import           Numeric.LinearAlgebra.HMatrix

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
             deriving (Generic, Show, Eq)

type IndicatorName = String

type PairwiseMatrix = Matrix Double

type PriorityVector = Matrix Double

-- * Alternatives definition

data Alternative = Alternative { altName   :: String
                               , indValues :: IndicatorValues
                               }
                 deriving (Generic, Show, Eq, Ord)

type IndicatorValues = Map IndicatorName Double

