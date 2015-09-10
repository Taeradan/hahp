module Configuration where

import Data.Packed.Matrix
import Data.Packed.Vector

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

isAHPConfigurationValid :: AHPTree -> Bool
isAHPConfigurationValid = isAHPTreeValid

isAHPTreeValid :: AHPTree -> Bool
isAHPTreeValid ahpNode = False

