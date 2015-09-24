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
showConfigurationSummary (ahpTree, validation) = unlines
    [ "# Configuration \"" ++ name ahpTree ++ "\""
    , ""
    , "## Aperçu de la configuration"
    , ""
    , showAhpTree ahpTree
    , ""
    , "## La configuration est elle valide ?"
    , ""
    , if validation
        then "-> configuration correcte"
        else "-> configuration invalide"
    ]

showAhpTree :: AHPTree -> String
showAhpTree = showAhpSubTree 0

showAhpSubTree :: Int -> AHPTree -> String
showAhpSubTree level (AHPTree name prefMatrix consistency childrenPriority _ children) = unlines
    [ tabs ++ "* Tree : " ++ name
    , tabs ++ "  matrice de préférence :"
    , showMatrix level prefMatrix
    , tabs ++ "  critère de cohérence = " ++ maybe "N/A" show consistency
    , tabs ++ "  vecteur de priorité :"
    , maybe "N/A" (showMatrix level) childrenPriority
    , concatMap (showAhpSubTree (level + 1)) children
    ]
        where tabs = variableTabs level
showAhpSubTree level (AHPLeaf name maximize _) = unlines
    [ tabs ++ "* Leaf : " ++ name
    , tabs ++ "  " ++ (if maximize then "maximize" else "minimize")
    ]
        where tabs = variableTabs level

showMatrix :: Int -> Matrix Double -> String
showMatrix level matrix = concatMap showMatrixLine lists
    where lists = toLists matrix
          showMatrixLine line = variableTabs level ++ "  | " ++
                                concatMap (\x -> printf "%.4f" x ++ " ") line ++
                                "|\n"

variableTabs :: Int -> String
variableTabs level = replicate level '\t'
