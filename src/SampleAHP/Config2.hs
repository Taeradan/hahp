module SampleAHP.Config2 where

import           Configuration
import           Data.Map
import           Data.Packed.Matrix

-- | Exemple pour "devenir maitre du monde"
sampleAHPConfig2 :: AHPTree
sampleAHPConfig2 = AHPTree
                        "Devenir le maitre du monde, Minus & Cortex"
                        ( fromLists [ [ 1.00, 1/4 , 4    ],
                                      [ 4   , 1.00, 9    ],
                                      [ 1/4 , 1/9 , 1.00 ]
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

