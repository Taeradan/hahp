module Configuration where

import Data.Matrix
import Data.Vector

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

