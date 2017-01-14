module HAHP.Sample.Config1 where

import           Data.Map
import           HAHP.Data
import           HAHP.Data.Core
import           Numeric.LinearAlgebra.HMatrix

rootName :: String
rootName = "Super objective"

rootPrefMatrix :: PairwiseMatrix
rootPrefMatrix = fromLists [  [1.0, 1.0] , [1.0, 1.0] ]

leaf1 :: AHPTree
leaf1 = AHPLeaf "Indicator 1" True Nothing

leaf2 :: AHPTree
leaf2 = AHPLeaf "Indicator 2" False Nothing

rootNodes :: [AHPTree]
rootNodes = [ leaf1, leaf2 ]

sampleAHPConfig1 :: AHPTree
sampleAHPConfig1 = AHPTree rootName rootPrefMatrix Nothing Nothing Nothing rootNodes

sampleIndicatorValues1 :: IndicatorValues
sampleIndicatorValues1 = insert "Indicator 1" 1
                    . insert "Indicator 2" 10
                        $ empty

sampleIndicatorValues1' :: IndicatorValues
sampleIndicatorValues1' = insert "Indicator 1" 10
                    . insert "Indicator 2" 10
                        $ empty

sampleIndicatorValues1'' :: IndicatorValues
sampleIndicatorValues1'' = insert "Indicator 1" 100
                    . insert "Indicator 2" 10
                        $ empty

sampleIndicatorValues1''' :: IndicatorValues
sampleIndicatorValues1''' = insert "Indicator 1" 100
                    . insert "Indicator 2" 1
                        $ empty

sampleIndicatorValues1'''' :: IndicatorValues
sampleIndicatorValues1'''' = insert "Indicator 1" 10
                    . insert "Indicator 2" 1000
                        $ empty

sampleIndicatorValues1''''' :: IndicatorValues
sampleIndicatorValues1''''' = insert "Indicator 1" 100
                    . insert "Indicator 2" 100
                        $ empty

alt1A = Alternative "alternative A" sampleIndicatorValues1
alt1B = Alternative "alternative B" sampleIndicatorValues1'
alt1C = Alternative "alternative C" sampleIndicatorValues1''
alt1D = Alternative "alternative D" sampleIndicatorValues1'''
alt1E = Alternative "alternative E" sampleIndicatorValues1''''
alt1F = Alternative "alternative F" sampleIndicatorValues1'''''

sampleAlternatives1 :: [Alternative]
sampleAlternatives1 =   [ alt1A
                        , alt1B
                        , alt1C
                        , alt1D
                        , alt1E
                        , alt1F
                        ]
