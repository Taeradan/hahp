module HAHP.Sample.LeaderChoice where

import           Data.Map
import           HAHP.Data.Core
import           Numeric.LinearAlgebra.HMatrix

leaderChoiceTree :: AHPTree
leaderChoiceTree = AHPTree "Choose the Most Suitable Leader"
                    ( (4><4) [   1,   4,   3, 7
                             , 1/4,   1, 1/3, 3
                             , 1/3,   3,   1, 5
                             , 1/7, 1/3, 1/5, 1
                             ]
                    )
                    Nothing
                    Nothing
                    Nothing
                    [ AHPLeaf "Experience" True Nothing
                    , AHPLeaf "Education" True Nothing
                    , AHPLeaf "Charisma" True Nothing
                    , AHPLeaf "Age" True Nothing
                    ]

tomValues =
      insert "Experience" 0.217
    . insert "Education" 0.188
    . insert "Charisma" 0.743
    . insert "Age" 0.265
    $ empty

dickValues =
      insert "Experience" 0.717
    . insert "Education" 0.081
    . insert "Charisma" 0.194
    . insert "Age" 0.672
    $ empty

harryValues =
      insert "Experience" 0.066
    . insert "Education" 0.731
    . insert "Charisma" 0.063
    . insert "Age" 0.063
    $ empty

leaderChoiceAlternatives :: [Alternative]
leaderChoiceAlternatives = [ Alternative "Tom" tomValues
                          , Alternative "Dick" dickValues
                          , Alternative "Harry" harryValues
                          ]
