module Algorithm where

import           Algorithm.Consistency
import           Algorithm.PriorityVector
import           Algorithm.Ranking
import           Algorithm.Validation
import           Configuration
import           Data.List
import           Data.Maybe
import           Data.Ord                      ( comparing )
import           Numeric.LinearAlgebra.HMatrix

initAHP :: AHPTree -> (AHPTree, Bool)
initAHP ahpTree = (newAHPTree, isTreeValid)
    where isTreeValid = isAHPTreeValid newAHPTree
          newAHPTree = computeTreePriorityVectors . computeTreeConsistencies $ ahpTree

rankAlternatives :: [Alternative] -> AHPTree -> [Alternative]
rankAlternatives alts ahpTree = map fst . sortOn' snd $ zip alts ranks
    where ranks = concat . toLists . fromJust $ alternativesPriority rankedAhpTree
          rankedAhpTree = computeTreeAlternativesPriorities alts ahpTree

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
