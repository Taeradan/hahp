module Configuration where

import Data.Matrix

type AHPConfiguration = AHPNode

type AHPNodes = [AHPNode]

data AHPNode = AHPTree { name :: String
                       , preferenceMatrix :: Matrix Double
                       , children :: AHPNodes
                       }
             | AHPLeave { name :: String
                        , maximize :: Bool
                        }
             | AHPNode
             deriving (Show)

