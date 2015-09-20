module SampleAHPConfig where

import           Configuration
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


sampleAHPConfig2 :: AHPTree
sampleAHPConfig2 = AHPTree
                        "Devenir le maitre du monde, Minus & Cortex"
                        ( fromLists [ [ 1.00, 3.00, 5.00 ],
                                      [ 0.33, 1.00, 9.00 ],
                                      [ 0.11, 0.20, 1.00 ]
                                    ]
                        )
                        Nothing
                        Nothing
                        Nothing
                        [ AHPLeaf "Puissance" True Nothing
                        , AHPTree
                            "Carisme"
                            ( (2><2) [ 1.0, 2
                                     , 0.5, 1.0 ]
                            )
                            Nothing
                            Nothing
                            Nothing
                            [ AHPLeaf "Timidité" False Nothing
                            , AHPLeaf "Tenue" True Nothing
                            ]
                        , AHPLeaf "Capital" True Nothing
                        ]
