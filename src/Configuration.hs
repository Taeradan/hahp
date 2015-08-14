module Configuration where

import Data.Packed.Matrix
import Data.Packed.Vector

type AHPConfiguration = AHPNode

type AHPNodes = [AHPNode]

type PreferenceMatrix = Matrix Double

type PriorityVector = Vector Double

data AHPNode = AHPTree { name :: String
                       , preferenceMatrix :: PreferenceMatrix
                       , children :: AHPNodes
                       }
             | AHPLeave { name :: String
                        , maximize :: Bool
                        }
             | AHPNode
             deriving (Show)

isAHPConfigurationValid :: AHPConfiguration -> Bool
isAHPConfigurationValid = isAHPNodeValid

isAHPNodeValid :: AHPNode -> Bool
isAHPNodeValid ahpNode = False

