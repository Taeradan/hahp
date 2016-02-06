module HAHP.Sample.CarChoice where

import Data.Map
import    HAHP.Data
import           Numeric.LinearAlgebra.HMatrix

carChoiceTree :: AHPTree
carChoiceTree =
  AHPTree "Choose the best car for the Jones family"
    ( (4><4)
        [   1,   3, 7, 3
        , 1/3,   1, 9, 1
        , 1/7, 1/9, 1, 1/7
        , 1/3,   1, 7, 1
        ]
    )
    Nothing
    Nothing
    Nothing
    [ AHPTree "Cost"
        ( (4><4)
            [   1,   2, 5, 3
            , 1/2,   1, 2, 2
            , 1/5, 1/2, 1, 1/2
            , 1/3, 1/2, 2, 1
            ]
        )
        Nothing
        Nothing
        Nothing
        [ AHPLeaf "Purchase Price" True Nothing
        , AHPLeaf "Fuel Costs" True Nothing
        , AHPLeaf "Maintenance Costs" True Nothing
        , AHPLeaf "Resale Value" True Nothing
        ]
    , AHPLeaf "Safety" True Nothing
    , AHPLeaf "Style" True Nothing
    , AHPTree "Capacity"
        ( (2><2)
            [ 1, 1/5
            , 5,   1
            ]
        )
        Nothing
        Nothing
        Nothing
        [ AHPLeaf "Cargo Capacity" True Nothing
        , AHPLeaf "Passenger Capacity" True Nothing
        ]
    ]

asValues =
      insert "Purchase Price" 0.242
    . insert "Fuel Costs" 0.188
    . insert "Maintenance Costs" 0.357
    . insert "Resale Value" 0.225
    . insert "Safety" 0.215
    . insert "Style" 0.346
    . insert "Cargo Capacity" 0.090
    . insert "Passenger Capacity" 0.136
    $ empty

ahValues =
      insert "Purchase Price" 0.027
    . insert "Fuel Costs" 0.212
    . insert "Maintenance Costs" 0.312
    . insert "Resale Value" 0.095
    . insert "Safety" 0.215
    . insert "Style" 0.346
    . insert "Cargo Capacity" 0.090
    . insert "Passenger Capacity" 0.136
    $ empty

psuvValues =
      insert "Purchase Price" 0.027
    . insert "Fuel Costs" 0.133
    . insert "Maintenance Costs" 0.084
    . insert "Resale Value" 0.055
    . insert "Safety" 0.083
    . insert "Style" 0.045
    . insert "Cargo Capacity" 0.170
    . insert "Passenger Capacity" 0.273
    $ empty

crvValues =
      insert "Purchase Price" 0.242
    . insert "Fuel Costs" 0.160
    . insert "Maintenance Costs" 0.100
    . insert "Resale Value" 0.415
    . insert "Safety" 0.038
    . insert "Style" 0.160
    . insert "Cargo Capacity" 0.170
    . insert "Passenger Capacity" 0.136
    $ empty

esuvValues =
      insert "Purchase Price" 0.362
    . insert "Fuel Costs" 0.151
    . insert "Maintenance Costs" 0.089
    . insert "Resale Value" 0.105
    . insert "Safety" 0.025
    . insert "Style" 0.025
    . insert "Cargo Capacity" 0.170
    . insert "Passenger Capacity" 0.046
    $ empty

omValues =
      insert "Purchase Price" 0.100
    . insert "Fuel Costs" 0.156
    . insert "Maintenance Costs" 0.058
    . insert "Resale Value" 0.105
    . insert "Safety" 0.424
    . insert "Style" 0.078
    . insert "Cargo Capacity" 0.310
    . insert "Passenger Capacity" 0.273
    $ empty

carChoiceAlternatives :: [Alternative]
carChoiceAlternatives = [ Alternative "Accord Sedan" asValues
                        , Alternative "Accord Hybrid" ahValues
                        , Alternative "Pilot SUV" psuvValues
                        , Alternative "CR-V SUV" crvValues
                        , Alternative "Element SUV" esuvValues
                        , Alternative "Odyssey Minivan" omValues
                        ]
