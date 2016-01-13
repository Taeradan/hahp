module HAHP.Reporting where

import           Data.List
import qualified Data.Map                      as M
import           Data.Time
import           HAHP.Data
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

-- | Print a simple report about an AHP tree and ranking result
simpleSummary :: (AHPTree, [Alternative], [ValidationError]) -- ^ AHP tree, some alternatives and the result of tree validation
              -> String                         -- ^ Report build from input
simpleSummary (ahpTree, alts, errors) =  treeSummary ++ altSummary ++ errorSummary ++ "\\newpage \n"
    where treeSummary = showConfiguration ahpTree
          altSummary = showAlternatives alts
          errorSummary = showErrors errors

-- | Print an AHP tree and some additional information about it
showConfiguration :: AHPTree -- ^ AHP tree
                         -> String  -- ^ Report about the AHP tree
showConfiguration ahpTree = unlines $
    [ "# Configuration \"" ++ name ahpTree ++ "\""
    , ""
    , "## AHP tree preview"
    , ""
    ]
    ++
    lines (showAhpTree ahpTree)

-- * Errors

showErrors :: [ValidationError] -- ^ List of errors
           -> String            -- ^ Report errors
showErrors errors = unlines $
    [ ""
    , "## AHP tree validity"
    , ""
    , "### Summary"
    , ""
    , if null errors
        then "This tree is valid"
        else "This tree is NOT valid"
    , ""
    , "### List of errors"
    , ""
    ]
    ++
    lines (concatMap showError errors)


showError :: ValidationError
          -> String
showError (ConsistencyError ahpTree consistencyTreshold consistency) =
    "* " ++ name ahpTree ++ ", consistency treshold = " ++ show consistencyTreshold ++ ", consistency value = " ++ printf "%.4f" consistency
showError (NotComputedConsistencyError ahpTree) =
    "* " ++ name ahpTree ++ ", consistency not computed !"
showError (SquareMatrixError ahpTree rows cols) =
    "* " ++ name ahpTree ++ ", matrix not square rows = " ++ show rows ++ ", columns = " ++ show cols

-- * Alternatives printing

-- | Print AHP Alternatives and some additional information about them
showAlternatives :: [Alternative] -- ^ AHP Alternatives
                 -> String        -- ^ Report alternatives with indicator values
showAlternatives alts = unlines $
    [ ""
    , "## Alternatives values"
    , ""
    ]
    ++
    lines (concatMap (showAlternative 0) alts)

-- | Print an AHP Alternative and some additional information about it
showAlternative :: Int         -- ^ Deep level. Used to intercalate separators
                -> Alternative -- ^ Alternative
                -> String      -- ^ Report about the alternative
showAlternative level a = unlines $
    [tabs ++ "1. " ++ altName a]
    ++
    lines (showIndicatorValues (level + 1) (indValues a))
  where tabs = variableTabs level

-- | Print an AHP IndicatorValues wit name and value
showIndicatorValues :: Int              -- ^ Deep level. Used to intercalate separators
                    -> IndicatorValues  -- ^ IndicatorValues
                    -> String           -- ^ Report about the values
showIndicatorValues level values = unlines $
    [ tabs ++ ""
    , tabs ++ "| Indicator | Value |"
    , tabs ++ "|-----------|-------|"
    ]
    ++
    map (showIndicatorValue level) (M.toList values)
    ++
    [""]
  where tabs = variableTabs level

showIndicatorValue :: Int -> (String, Double) -> String
showIndicatorValue level (key, value) = tabs ++ "| " ++ key ++ " | " ++ show value ++ " |"
    where tabs = variableTabs level

-- * AHP tree printing

showAhpTree :: AHPTree -> String
showAhpTree = showAhpSubTree 0

showAhpSubTree :: Int -> AHPTree -> String
showAhpSubTree level (AHPTree name prefMatrix consistency childrenPriority alternativesPriority children) = unlines $
    [ tabs ++ "* Tree : " ++ name
    , tabs ++ "\t- pairwise comparison matrix :"
    , tabs
    ]
    ++
    lines (showMatrix (level + 2) prefMatrix)
    ++
    [ tabs ++ "\t- consistency ratio = " ++ maybe "N/A" show consistency
    , tabs ++ "\t- children priority vector :"
    , tabs
    ]
    ++
    lines (maybe ((variableTabs $ level + 2) ++ "N/A") (showMatrix (level + 2)) childrenPriority)
    ++
    [ tabs ++ "\t- alternatives priority vector :"
    , tabs
    ]
    ++
    lines (maybe ((variableTabs $ level + 2) ++ "N/A") (showMatrix (level + 2)) alternativesPriority)
    ++
    lines (concatMap (showAhpSubTree (level + 1)) children)
        where tabs = variableTabs level

showAhpSubTree level (AHPLeaf name maximize alternativesPriority) = unlines $
    [ tabs ++ "* Leaf : " ++ name
    , tabs ++ "\t- " ++ (if maximize then "indicator is maximized" else "indicator is minimized")
    , tabs ++ "\t- alternatives priority vector :"
    , tabs
    ]
    ++
    lines (maybe ((variableTabs $ level + 2) ++ "N/A") (showMatrix (level + 1)) alternativesPriority)
        where tabs = variableTabs level

-- * Matrix printing

showMatrix :: Int -> Matrix Double -> String
showMatrix level = showMatrix' (level + 2)

showMatrix' :: Int -> Matrix Double -> String
showMatrix' level matrix = concatMap showMatrixLine lists
    where lists = toLists matrix
          showMatrixLine line = variableTabs level ++ "  | " ++
                                concatMap (\x -> printf "%.4f" x ++ " ") line ++
                                "|\n"

variableTabs :: Int -> String
variableTabs level = replicate level '\t'
