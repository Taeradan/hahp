module Configuration where

import           Numeric.LinearAlgebra.HMatrix

data AHPTree = AHPTree { name                 :: String
                       , preferenceMatrix     :: PreferenceMatrix
                       , consistencyValue     :: Maybe Double
                       , childrenPriority     :: Maybe PriorityVector
                       , alternativesPriority :: Maybe PriorityVector
                       , children             :: [AHPTree]
                       }
             | AHPLeaf { name                  :: String
                        , maximize             :: Bool
                        , alternativesPriority :: Maybe PriorityVector
                        }
             deriving (Show)

type PreferenceMatrix = Matrix Double

type PriorityVector = Matrix Double
