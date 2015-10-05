module SampleAHP.Config1 where

import           Configuration
import           Data.Map
import           Data.Packed.Matrix

rootName :: String
rootName = "Super objectif"

rootPrefMatrix :: PreferenceMatrix
rootPrefMatrix = fromLists [  [1.0, 1.0] , [1.0, 1.0] ]

leaf1 :: AHPTree
leaf1 = AHPLeaf "Indicateur 1" True Nothing

leaf2 :: AHPTree
leaf2 = AHPLeaf "Indicateur 2" False Nothing

rootNodes :: [AHPTree]
rootNodes = [ leaf1, leaf2 ]

sampleAHPConfig :: AHPTree
sampleAHPConfig = AHPTree rootName rootPrefMatrix Nothing Nothing Nothing rootNodes

