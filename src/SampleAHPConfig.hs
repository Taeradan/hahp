module SampleAHPConfig where
    
import Configuration
import Data.Matrix

rootName :: String
rootName = "Super objectif"

rootPrefMatrix :: Matrix Double
rootPrefMatrix = fromLists [  [1.0, 1.0] , [1.0, 1.0] ]

leave1 :: AHPNode
leave1 = AHPLeave "Indicateur 1" True

leave2 :: AHPNode
leave2 = AHPLeave "Indicateur 2" False

rootNodes :: AHPNodes
rootNodes = [ leave1, leave2 ]

sampleAHPConfig :: AHPNode
sampleAHPConfig = AHPTree rootName rootPrefMatrix rootNodes

