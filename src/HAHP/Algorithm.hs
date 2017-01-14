module HAHP.Algorithm where

import           Data.List
import           Data.Maybe
import           Data.Ord                      (comparing)
import           HAHP.Algorithm.Consistency
import           HAHP.Algorithm.PriorityVector
import           HAHP.Algorithm.Ranking
import           HAHP.Data
import           HAHP.Data.Core
import           HAHP.Data.Errors
import           HAHP.Validation.Alternatives
import           HAHP.Validation.Tree
import           Numeric.LinearAlgebra.HMatrix

-- |This function is a quick way to rank a set of alternatives with AHP algorithm.
-- This function call everithing required to configure an execute AHP process.
-- If something goes wrong, an error is raised.
simpleAHP :: AHPDataSet
          -> (AHPDataSet, [TreeError], [AlternativesError])
simpleAHP inputDataSet =
    if null inputTreeErrors && null altsErrors
      then (processedDataSet, treeErrors, [])
      else (inputDataSet, inputTreeErrors ++ treeErrors, altsErrors)
    where (ahpTree, alts) = inputDataSet
          initializedTree = initAHP ahpTree
          processedDataSet = rankAlternatives (initializedTree, alts)
          ---
          inputTreeErrors = validateInputAHPTree ahpTree
          altsErrors = validateAlternatives inputDataSet
          treeErrors = if null inputTreeErrors
                         then validateAHPTree initializedTree
                         else []

-- * Part 1 = static part

initAHP :: AHPTree
        -> AHPTree
initAHP = computeTreePriorityVectors . computeTreeConsistencies

-- * Part 2 = dynamic part

rankAlternatives :: AHPDataSet
                 -> AHPDataSet
rankAlternatives (ahpTree, alts) = (rankedAhpTree, sortedRankedAlternatives)
    where ranks = concat . toLists . fromJust . alternativesPriority $ rankedAhpTree
          rankedAhpTree = computeTreeAlternativesPriorities alts ahpTree
          sortedRankedAlternatives = reverse . map fst . sortOn' snd $ zip alts ranks

-- | Sort a list by comparing the results of a key function applied to each
-- element.  @sortOn f@ is equivalent to @sortBy . comparing f@, but has the
-- performance advantage of only evaluating @f@ once for each element in the
-- input list.  This is called the decorate-sort-undecorate paradigm, or
-- Schwartzian transform.
-- see https://hackage.haskell.org/package/base-4.8.1.0/docs/src/Data.OldList.html#sortOn
--
-- TODO : Remote this function and use default haskell sortOn.
-- Note: Compilation issues on mac. sortOn is not provided.
--
-- @since 4.8.0.0
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))
