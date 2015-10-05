module Reporting where

import           Configuration
import           Data.List
import qualified Data.Map                      as M
import           Data.Time
import           Numeric.LinearAlgebra.HMatrix
import           Text.Printf

-- * Report making

-- | Given a title, an author and a timestamp, builds a Pandoc Markdown document header
reportHeader :: String  -- ^ Report title
             -> String  -- ^ Report author
             -> UTCTime -- ^ Report timestamp
             -> String  -- ^ Report header

reportHeader title author time = unlines
    [ "% " ++ title
    , "% " ++ author
    , "% " ++ showGregorian(utctDay time)
    ]

-- | Print an AHP tree and some additional information about it
showConfigurationSummary :: (AHPTree, Bool) -- ^ AHP tree and the result of its validation
                         -> String          -- ^ Report about the AHP tree

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

-- * Alternatives printing

showAlternatives :: [Alternative] -> String
showAlternatives alts = unlines
    [ "## Valeur des alternatives"
    , ""
    , concatMap (showAlternative 1) alts
    ]

showAlternative :: Int -> Alternative -> String
showAlternative level a = unlines
    [ tabs ++ "* " ++ altName a
    , tabs ++ "  " ++ (showIndicatorValues level (indValues a))
    ]
        where tabs = variableTabs level

showIndicatorValues :: Int -> IndicatorValues -> String
showIndicatorValues level values = unlines
    [ tabs ++ "valeurs des indicateurs :"
    , unlines $ map (showIndicatorValue level) (M.toList values)
    ]
        where tabs = variableTabs (level)

showIndicatorValue :: Int -> (String, Double) -> String
showIndicatorValue level (key, value) = tabs ++ "* " ++ key ++ " = " ++ show value
    where tabs = variableTabs (level + 1)

-- * AHP tree printing

showAhpTree :: AHPTree -> String
showAhpTree = showAhpSubTree 0

showAhpSubTree :: Int -> AHPTree -> String
showAhpSubTree level (AHPTree name prefMatrix consistency childrenPriority alternativesPriority children) = unlines
    [ tabs ++ "* Tree : " ++ name
    , tabs ++ "  matrice de préférence :"
    , showMatrix level prefMatrix
    , tabs ++ "  critère de cohérence = " ++ maybe "N/A" show consistency
    , tabs ++ "  vecteur de priorité :"
    , maybe "N/A" (showMatrix level) childrenPriority
    , tabs ++ "  priorité entre alternatives :"
    , maybe "N/A" (showMatrix level) alternativesPriority
    , concatMap (showAhpSubTree (level + 1)) children
    ]
        where tabs = variableTabs level

showAhpSubTree level (AHPLeaf name maximize alternativesPriority) = unlines
    [ tabs ++ "* Leaf : " ++ name
    , tabs ++ "  " ++ (if maximize then "maximize" else "minimize")
    , tabs ++ "  priorité entre alternatives :"
    , maybe "N/A" (showMatrix level) alternativesPriority
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
