-- | Example from Ounnar 1999
module HAHP.Sample.SquareMatrixError where

import           Data.Map
import           HAHP.Data
import           Numeric.LinearAlgebra.HMatrix

smeConfig :: AHPTree
smeConfig = AHPTree
                       "Testing the Square Matrix Error"
                       ( (4><5) [   1,  1/5,   0, 3, 2
                                ,   5,    2,  -3, 5, 2
                                ,   1, -1/3,   1, 4, 2
                                , 1/3,  1/5, 1/3, 1, 2
                                ]
                       )
                       Nothing
                       Nothing
                       Nothing
                       [ AHPLeaf "d" True Nothing
                       , AHPLeaf "Tp" True Nothing
                       , AHPLeaf "Tp" True Nothing
                       , AHPLeaf "Id" False Nothing
                       , AHPLeaf "Id" False Nothing
                       ]

smeIndicatorValues :: IndicatorValues
smeIndicatorValues = insert "d" 1
                   . insert "Tp" 10
                   . insert "Tf" 100
                   . insert "Id" 1000
                   $ empty

smeIndicatorValues' :: IndicatorValues
smeIndicatorValues' = insert "d" 2
                    . insert "Tp" 20
                    . insert "Tf" 200
                    . insert "Id" 2000
                    $ empty

smeIndicatorValues'' :: IndicatorValues
smeIndicatorValues'' = insert "d" 1
                     . insert "Tp" 10
                     . insert "Tf" 100
                     . insert "Id" 2000
                     $ empty

smeAlternatives :: [Alternative]
smeAlternatives = [ Alternative "alternative A" smeIndicatorValues
                  , Alternative "alternative B" smeIndicatorValues'
                  , Alternative "alternative B" smeIndicatorValues'
                  , Alternative "alternative C" smeIndicatorValues''
                  ]
