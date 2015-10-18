module HAHP.Algorithm.Ranking where

import           Data.List
import qualified Data.Map                      as M
import           Data.Maybe
import           Debug.Trace
import           HAHP.Algorithm.PriorityVector
import           HAHP.Data
import           Numeric.LinearAlgebra.HMatrix

computeTreeAlternativesPriorities :: [Alternative] -> AHPTree -> AHPTree
computeTreeAlternativesPriorities alts ahpTree =
    case ahpTree of
        (AHPTree _ _ _ _ _ children) -> agregateTreeAlternativesPriorities . computeChildrenTreeAlternativesPriorities alts $ ahpTree
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
        children = map (computeTreeAlternativesPriorities alts) (children ahpTree)
    }

-- * Computation function

agregatePriorities :: AHPTree -> PriorityVector
agregatePriorities ahpTree = catChildVectors <> childPriorities
    where childVectors = map (fromJust . alternativesPriority) (children ahpTree)
          catChildVectors = foldl1 (|||) childVectors
          childPriorities = fromJust . childrenPriority $ ahpTree

computeAlternativesPriority :: AHPTree -> [Alternative] -> PriorityVector
computeAlternativesPriority ahpTree alts = result
    where pairwiseAlternatives =  buildAlternativePairwiseMatrix ahpTree alts
          result = priorityVector pairwiseAlternatives

buildAlternativePairwiseMatrix :: AHPTree -> [Alternative] -> Matrix Double
buildAlternativePairwiseMatrix ahpTree alts = (length alts >< length alts) matrix
        where vals = map (selectIndValue (name ahpTree)) alts
	      cartesianProduct = [(x, y) | x <- vals, y <- vals]
              matrix = map operator cartesianProduct
	      operator = if maximize ahpTree
                         then (\ (x, y) -> x / y)
                         else (\ (x, y) -> y / x)

-- * Extract data from data stuctures

selectIndValue :: IndicatorName -> Alternative -> Double
selectIndValue name alt = selectIndValue' name (M.toList . indValues $ alt)

selectIndValue' :: IndicatorName -> [(IndicatorName, Double)] -> Double
selectIndValue' name values = fromJust (lookup name values)
