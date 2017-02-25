module HAHP.Algorithm.Ranking where

import           Control.Parallel.Strategies
import           Data.List
import qualified Data.Map                      as M
import           Data.Maybe
import           Debug.Trace
import           HAHP.Algorithm.PriorityVector
import           HAHP.Data
import           HAHP.Data.Core
import           HAHP.Data.Errors
import           Numeric.LinearAlgebra.HMatrix

computeTreeAlternativesPriorities :: [Alternative] -> AHPTree -> AHPTree
computeTreeAlternativesPriorities alts ahpTree =
    case ahpTree of
        AHPTree {} -> agregateTreeAlternativesPriorities . computeChildrenTreeAlternativesPriorities alts $ ahpTree
        AHPLeaf {} -> ahpTree
            { alternativesPriority = Just $ computeAlternativesPriority ahpTree alts
            }

-- * Helper function.

agregateTreeAlternativesPriorities :: AHPTree -> AHPTree
agregateTreeAlternativesPriorities ahpTree = ahpTree {
        alternativesPriority = Just . agregatePriorities $ ahpTree
    }

computeChildrenTreeAlternativesPriorities :: [Alternative] -> AHPTree -> AHPTree
computeChildrenTreeAlternativesPriorities alts ahpTree = ahpTree {
        children = parMap rseq (computeTreeAlternativesPriorities alts) (children ahpTree)
    }

-- * Computation function

agregatePriorities :: AHPTree -> PriorityVector
agregatePriorities ahpTree = catChildVectors <> childPriorities
    where childVectors = parMap rseq (fromJust . alternativesPriority) (children ahpTree)
          catChildVectors = foldl1 (|||) childVectors
          childPriorities = fromJust . childrenPriority $ ahpTree

computeAlternativesPriority :: AHPTree -> [Alternative] -> PriorityVector
computeAlternativesPriority ahpTree alts = result
    where pairwiseAlternatives =  buildAlternativePairwiseMatrix ahpTree alts
          result = priorityVector pairwiseAlternatives

buildAlternativePairwiseMatrix :: AHPTree -> [Alternative] -> Matrix Double
buildAlternativePairwiseMatrix ahpTree alts = (length alts >< length alts) matrix
        where vals = parMap rseq (selectIndValue (name ahpTree)) alts
              cartesianProduct = [(x, y) | x <- vals, y <- vals]
              -- matrix = parMap rseq operator cartesianProduct
              matrix = map operator cartesianProduct
              operator = if maximize ahpTree
                         -- `uncurry` permit the use of an operator on a pair
                         then uncurry (/)
                         -- `flip` revert the arguments
                         else uncurry (flip (/))

-- * Extract data from data stuctures

selectIndValue :: IndicatorName -> Alternative -> Double
selectIndValue name alt = selectIndValue' name (M.toList . indValues $ alt)

selectIndValue' :: IndicatorName -> [(IndicatorName, Double)] -> Double
selectIndValue' name values = fromJust (lookup name values)
