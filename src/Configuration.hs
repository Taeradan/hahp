module Configuration where

import Data.Matrix

type AHPConfiguration = AHPNode

data AHPNode = AHPTree { name :: String
                       , preferenceMatrix :: Matrix Double
                       , children :: [AHPNode]
                       }
             | AHPLeave { name :: String
                        , maximize :: Bool
                        }
             deriving (Show)

