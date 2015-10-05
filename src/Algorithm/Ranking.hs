module Algorithm.Ranking where

import           Algorithm.PriorityVector
import           Configuration
import           Data.List
import qualified Data.Map                      as M
import           Data.Maybe
import           Debug.Trace
import           Numeric.LinearAlgebra.HMatrix
import           Reporting

computeTreeAlternativesPriorities :: [Alternative] -> AHPTree -> AHPTree
computeTreeAlternativesPriorities alts ahpTree =
    case ahpTree of
        (AHPTree _ _ _ _ _ children) -> agregateTreeAlternativesPriorities . computeChildrenTreeAlternativesPriorities alts $ ahpTree
--        AHPLeaf {} -> ahpTree
        AHPLeaf {name = name} -> ahpTree
            { alternativesPriority = Just $ computeAlternativesPriority alts name
            }

-- * Helper function.

agregateTreeAlternativesPriorities :: AHPTree -> AHPTree
agregateTreeAlternativesPriorities ahpTree = ahpTree {
	--alternativesPriority = Just . trace ( "###### agregatePriorities de " ++ showAhpTree ahpTreeWithChildren) . agregatePriorities $ ahpTreeWithChildren
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

computeAlternativesPriority :: [Alternative] -> IndicatorName -> PriorityVector
--computeAlternativesPriority alts name = (trace ("Affichage vecteur priorite pour " ++ name ++ show result)) $ result
computeAlternativesPriority alts name = result
    where pairwiseAlternatives =  buildAlternativePairwiseMatrix name alts alts
          --result = priorityVector . (trace ("Affichage matrice Alt x Alt pour " ++ name ++ show pairwiseAlternatives)) $ pairwiseAlternatives
          result = priorityVector pairwiseAlternatives

-- TODO : implements Minimize or Maximize option
buildAlternativePairwiseMatrix :: IndicatorName -> [Alternative] -> [Alternative] -> Matrix Double
buildAlternativePairwiseMatrix name altsA altsB = (length altsA >< length altsB)matrix
        where valsA = map (selectIndValue name) altsA
	      valsB = map (selectIndValue name) altsB
	      cartesianProduct = [(x,y) | x <- valsA, y <- valsB]
              --matrix = map divideMinimize cartesianProduct
              matrix = map divideMaximize cartesianProduct

divideMaximize :: (Double, Double) -> Double
divideMaximize (x,y) = x / y

divideMinimize :: (Double, Double) -> Double
divideMinimize (x,y) = y / x

-- * Extract data from data stuctures

selectIndValue :: IndicatorName -> Alternative -> Double
selectIndValue name value = selectIndValue' name (M.toList . indValues $ value)

selectIndValue' :: IndicatorName -> [(IndicatorName, Double)] -> Double
selectIndValue' name values = fromJust (lookup name values)
