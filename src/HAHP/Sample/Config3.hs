-- | Example from Ounnar 1999
module HAHP.Sample.Config3 where

import           Data.Map
import           HAHP.Data
import           Numeric.LinearAlgebra.HMatrix

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
                       , AHPLeaf "Id" False Nothing
                       ]

sampleIndicatorValues3 :: IndicatorValues
sampleIndicatorValues3 = insert "d" 1
                    . insert "Tp" 10
                    . insert "Tf" 100
                    . insert "Id" 1000
                        $ empty

sampleIndicatorValues3' :: IndicatorValues
sampleIndicatorValues3' = insert "d" 2
                    . insert "Tp" 20
                    . insert "Tf" 200
                    . insert "Id" 2000
                        $ empty

sampleIndicatorValues3'' :: IndicatorValues
sampleIndicatorValues3'' = insert "d" 1
                    . insert "Tp" 10
                    . insert "Tf" 100
                    . insert "Id" 2000
                        $ empty

alt3A = Alternative "alternative A" sampleIndicatorValues3
alt3B = Alternative "alternative B" sampleIndicatorValues3'
alt3C = Alternative "alternative C" sampleIndicatorValues3''

sampleAlternatives3 ::  [Alternative]
sampleAlternatives3 =   [ alt3A
                        , alt3B
                        , alt3C
                        ]
