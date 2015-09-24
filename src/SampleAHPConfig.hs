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
                        ( fromLists [ [ 1.00, 1.00, 1.00 ],
                                      [ 1.00, 1.00, 1.00 ],
                                      [ 1.00, 1.00, 1.00 ]
                                    ]
                        )
                        Nothing
                        Nothing
                        Nothing
                        [ AHPLeaf "Puissance" True Nothing
                        , AHPTree
                            "Carisme"
                            ( (3><3) [ 1,   3,   5
                                     , 1/3, 1,   9
                                     , 1/5, 1/9, 1
                                     ]
                            )
                            Nothing
                            Nothing
                            Nothing
                            [ AHPLeaf "Timidité" False Nothing
                            , AHPLeaf "Tenue" True Nothing
                            , AHPLeaf "Taille de la *" True Nothing
                            ]
                        , AHPLeaf "Capital" True Nothing
                        ]

sampleAHPConfig3 :: AHPTree
sampleAHPConfig3 = AHPTree
                       "Testing the Priority vectors computation"
                       ( (4><4) [ 1,   1/5, 1,   3
                                , 5,   1,   3,   5
                                , 1,   1/3, 1,   3
                                , 1/3, 1/5, 1/3, 1
                                ]
                       )
                       Nothing
                       Nothing
                       Nothing
                       [ AHPLeaf "d" True Nothing
                       , AHPLeaf "Tp" True Nothing
                       , AHPLeaf "Tf" True Nothing
                       , AHPLeaf "Id" True Nothing
                       ]
