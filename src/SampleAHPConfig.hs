module SampleAHPConfig where
    
import Configuration
import Data.Matrix

rootName :: String
rootName = "Super objectif"

rootPrefMatrix :: Matrix Double
rootPrefMatrix = fromLists [  [1.0, 1.0] , [1.0, 1.0] ]

rootLeafs :: [AHPLeave]
rootLeafs = [ AHPLeave ( "Indicateur 1", True  ), AHPLeave ( "Indicateur 2", False ) ]

sampleAHPConfig = AHPTree ( rootName
                          , rootPrefMatrix
                          , rootLeafs
                          )

