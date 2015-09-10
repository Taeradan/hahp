module Configuration where

import Numeric.LinearAlgebra.HMatrix

data AHPTree = AHPTree { name :: String
                       , preferenceMatrix :: PreferenceMatrix
                       , priorityVector :: Maybe PriorityVector
                       , children :: [AHPTree]
                       }
             | AHPLeave { name :: String
                        , maximize :: Bool
                        }
             deriving (Show)

type PreferenceMatrix = Matrix Double

type PriorityVector = Vector Double
