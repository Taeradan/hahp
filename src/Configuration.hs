module Configuration where

import           Data.List
import           Numeric.LinearAlgebra.HMatrix
import           Text.Printf

data AHPTree = AHPTree { name                 :: String
                       , preferenceMatrix     :: PreferenceMatrix
                       , consistencyValue     :: Maybe Double
                       , childrenPriority     :: Maybe PriorityVector
                       , alternativesPriority :: Maybe PriorityVector
                       , children             :: [AHPTree]
                       }
             | AHPLeaf { name                  :: String
                        , maximize             :: Bool
                        , alternativesPriority :: Maybe PriorityVector
                        }
             deriving (Show)

type PreferenceMatrix = Matrix Double

type PriorityVector = Vector Double

showAhpTree :: AHPTree -> String
showAhpTree = showAhpSubTree 0

showAhpSubTree :: Int -> AHPTree -> String
showAhpSubTree level (AHPTree name prefMatrix consistency childrenPriority _ children) =
    concat
    [ tabs ++ "* Tree : " ++ name ++ "\n"
    , tabs ++ "  critère de cohérence = " ++ maybe "N/A" show consistency ++ "\n"
    , tabs ++ "  vecteur de priorité = " ++ maybe "N/A" (\ x -> show x ++ "\n") childrenPriority ++ "\n"
    , tabs ++ "  matrice de préférence :\n"
    , showMatrix level prefMatrix ++ "\n"
    , concatMap (showAhpSubTree (level + 1)) children
    , "\n"
    ]
        where tabs = variableTabs level
showAhpSubTree level (AHPLeaf name maximize _) =
    concat
    [ tabs ++ "* Leaf : " ++ name ++ "\n"
    , tabs ++ "  " ++ (if maximize then "maximize" else "minimize") ++ "\n"
    ]
        where tabs = variableTabs level


variableTabs :: Int -> String
variableTabs level = replicate level '\t'

showMatrix :: Int -> PreferenceMatrix -> String
showMatrix level matrix = concatMap showMatrixLine lists
    where lists = toLists matrix
          showMatrixLine line = variableTabs level ++ "  | " ++
                                concatMap (\x -> printf "%.2f" x ++ " ") line ++
                                "|\n"
