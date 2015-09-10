module SampleAHPConfig where

import Configuration
import Data.Packed.Matrix

rootName :: String
rootName = "Super objectif"

rootPrefMatrix :: PreferenceMatrix
rootPrefMatrix = fromLists [  [1.0, 1.0] , [1.0, 1.0] ]

leave1 :: AHPTree
leave1 = AHPLeave "Indicateur 1" True

leave2 :: AHPTree
leave2 = AHPLeave "Indicateur 2" False

rootNodes :: [AHPTree]
rootNodes = [ leave1, leave2 ]

sampleAHPConfig :: AHPTree
sampleAHPConfig = AHPTree rootName rootPrefMatrix Nothing rootNodes


sampleAHPConfig2 :: AHPTree
sampleAHPConfig2 = AHPTree
                        "Devenir le maitre du monde, Minus & Cortex"
                        ( fromLists [ [ 1.00, 3.00, 5.00 ],
                                      [ 0.33, 1.00, 9.00 ],
                                      [ 0.11, 0.20, 1.00 ]
                                    ]
                        )
                        Nothing
                        [
                            AHPLeave "Puissance" True
                            , AHPLeave "Carisme" True
                            , AHPLeave "Capital" True
                        ]
