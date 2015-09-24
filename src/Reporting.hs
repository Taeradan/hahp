module Reporting where

import           Configuration
import           Data.List
import           Data.Time
import           Numeric.LinearAlgebra.HMatrix
import           Text.Printf

reportHeader :: String -> String -> UTCTime -> String
reportHeader title author time = unlines
    [ "% " ++ title
    , "% " ++ author
    , "% " ++ showGregorian(utctDay time)
    ]

showConfigurationSummary :: (AHPTree, Bool) -> String
showConfigurationSummary (ahpTree, validation) = concat
    [ "# Configuration \"" ++ name ahpTree ++ "\"\n"
    , "\n"
    , "## Aperçu de la configuration\n"
    , "\n"
    , showAhpTree ahpTree ++ "\n"
    , "\n"
    , "## La configuration est elle valide ?\n"
    , "\n"
    , if validation
        then "-> configuration correcte\n"
        else "-> configuration invalide\n"
    ]

showAhpTree :: AHPTree -> String
showAhpTree = showAhpSubTree 0

showAhpSubTree :: Int -> AHPTree -> String
showAhpSubTree level (AHPTree name prefMatrix consistency childrenPriority _ children) =
    concat
    [ tabs ++ "* Tree : " ++ name ++ "\n"
    , tabs ++ "  matrice de préférence :\n"
    , showMatrix level prefMatrix
    , tabs ++ "  critère de cohérence = " ++ maybe "N/A" show consistency ++ "\n"
    , tabs ++ "  vecteur de priorité :\n"
    , maybe "N/A" (showMatrix level) childrenPriority ++ "\n"
    , concatMap (showAhpSubTree (level + 1)) children
    ]
        where tabs = variableTabs level
showAhpSubTree level (AHPLeaf name maximize _) =
    concat
    [ tabs ++ "* Leaf : " ++ name ++ "\n"
    , tabs ++ "  " ++ (if maximize then "maximize" else "minimize") ++ "\n"
    ]
        where tabs = variableTabs level

showMatrix :: Int -> (Matrix Double) -> String
showMatrix level matrix = concatMap showMatrixLine lists
    where lists = toLists matrix
          showMatrixLine line = variableTabs level ++ "  | " ++
                                concatMap (\x -> printf "%.4f" x ++ " ") line ++
                                "|\n"

variableTabs :: Int -> String
variableTabs level = replicate level '\t'
