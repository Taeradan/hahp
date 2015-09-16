module Configuration where

import Data.List
import Numeric.LinearAlgebra.HMatrix
import Text.Printf

data AHPTree = AHPTree { name :: String
                       , preferenceMatrix :: PreferenceMatrix
                       , priorityVector :: Maybe PriorityVector
                       , children :: [AHPTree]
                       }
             | AHPLeaf { name :: String
                        , maximize :: Bool
                        }
             deriving (Show)

type PreferenceMatrix = Matrix Double

type PriorityVector = Vector Double

showAhpTree :: AHPTree -> String
showAhpTree = showAhpSubTree 0

showAhpSubTree :: Int -> AHPTree -> String
showAhpSubTree level (AHPTree name prefMatrix prioVector children) =
    intercalate (variableTabs  level)
    [ ""
    , "* Tree : " ++ name ++ "\n"
    , show prefMatrix ++ "\n"
    , maybe "" show prioVector ++ "\n"
    , concatMap (showAhpSubTree (level + 1)) children
    ]
showAhpSubTree level (AHPLeaf name maximize) =
    intercalate (variableTabs level)
    [ ""
    , "* Leaf : " ++ name ++ "\n"
    , (if maximize then "maximize" else "minimize") ++ "\n"
    ]

variableTabs :: Int -> String
variableTabs level = replicate level '\t'

showMatrix :: Int -> PreferenceMatrix -> String
showMatrix level matrix = concatMap showMatrixLine lists
    where lists = toLists matrix
          showMatrixLine line = variableTabs level ++ "  | " ++
                                concatMap (\x -> printf "%.2f" x ++ " ") line ++
                                "|\n"
