module SampleAHPConfig where
    
import Configuration
import Data.Matrix

rootName :: String
rootName = "Super objectif"

rootPrefMatrix :: PreferenceMatrix
rootPrefMatrix = fromLists [  [1.0, 1.0] , [1.0, 1.0] ]

leave1 :: AHPNode
leave1 = AHPLeave "Indicateur 1" True

leave2 :: AHPNode
leave2 = AHPLeave "Indicateur 2" False

rootNodes :: AHPNodes
rootNodes = [ leave1, leave2 ]

sampleAHPConfig :: AHPNode
sampleAHPConfig = AHPTree rootName rootPrefMatrix rootNodes


sampleAHPConfig2 :: AHPConfiguration
sampleAHPConfig2 = AHPTree
                        "Devenir le maitre du monde, Minus & Cortex"
                        ( fromLists [ [ 1.00, 3.00, 5.00 ],
                                      [ 0.33, 1.00, 9.00 ],
                                      [ 0.11, 0.20, 1.00 ]
                                    ]
                        )
                        (
                            [
                                AHPLeave "Puissance" True
                                , AHPLeave "Carisme" True
                                , AHPLeave "Capital" True
                            ]
                        )

