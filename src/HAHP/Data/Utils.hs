module HAHP.Data.Utils where

import           Data.Map                      (Map)
import           GHC.Generics
import           Numeric.LinearAlgebra.HMatrix

import           HAHP.Data.Core

-- * Data retrieve functions

getTreeLeaves :: AHPTree   -- ^ Input tree
              -> [AHPTree] -- ^ List of the leaves
getTreeLeaves ahpTree =
    case ahpTree of
      AHPTree {} -> concatMap getTreeLeaves (children ahpTree)
      AHPLeaf {} -> [ahpTree]

getIndicatorCurrentLevelCount   :: AHPTree  -- ^ Input tree
                                -> Int      -- ^ Indicator (AHPLeaf) count. Current level only
getIndicatorCurrentLevelCount ahpTree = length leaves
    where leaves = filter isLeaf (children ahpTree)

getIndicatorRecursiveCount  :: AHPTree -- ^ Input tree
                            -> Int     -- ^ Indicator (AHPLeaf) recursively counted
getIndicatorRecursiveCount ahpTree =
    case ahpTree of
        AHPLeaf {} -> 1
        AHPTree {} -> currentLevel + countSubLevel
    where trees = filter isTree (children ahpTree)
          currentLevel = getIndicatorCurrentLevelCount ahpTree
          countSubLevel = sum $ map getIndicatorRecursiveCount trees

isLeaf :: AHPTree -> Bool
isLeaf ahpTree =
    case ahpTree of
        AHPLeaf {} -> True
        AHPTree {} -> False

isTree :: AHPTree -> Bool
isTree = not . isLeaf

data IndicatorCountTree  =
    IndicatorCountTree  { indCurrentCount :: Int
                        , indChildren     :: [IndicatorCountTree]
                        }

