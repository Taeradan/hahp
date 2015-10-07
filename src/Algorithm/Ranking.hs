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
            { alternativesPriority = Just $ computeAlternativesPriority ahpTree alts name
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

computeAlternativesPriority :: AHPTree -> [Alternative] -> IndicatorName -> PriorityVector
--computeAlternativesPriority alts name = (trace ("Affichage vecteur priorite pour " ++ name ++ show result)) $ result
computeAlternativesPriority ahpTree alts name = result
    where pairwiseAlternatives =  buildAlternativePairwiseMatrix ahpTree name alts alts
          --result = priorityVector . (trace ("Affichage matrice Alt x Alt pour " ++ name ++ show pairwiseAlternatives)) $ pairwiseAlternatives
          result = priorityVector pairwiseAlternatives

buildAlternativePairwiseMatrix :: AHPTree -> IndicatorName -> [Alternative] -> [Alternative] -> Matrix Double
buildAlternativePairwiseMatrix ahpTree name altsA altsB = (length altsA >< length altsB)matrix
        where valsA = map (selectIndValue name) altsA
	      valsB = map (selectIndValue name) altsB
	      cartesianProduct = [(x,y) | x <- valsA, y <- valsB]
              matrix = map operator cartesianProduct
	      operator = (if isMaximize then divideMaximize else divideMinimize)
	      isMaximize = maximize . fromJust . extractLeaf ahpTree $ name

divideMaximize :: (Double, Double) -> Double
divideMaximize (x,y) = x / y

divideMinimize :: (Double, Double) -> Double
divideMinimize (x,y) = y / x

-- * Extract data from data stuctures

selectIndValue :: IndicatorName -> Alternative -> Double
selectIndValue name value = selectIndValue' name (M.toList . indValues $ value)

selectIndValue' :: IndicatorName -> [(IndicatorName, Double)] -> Double
selectIndValue' name values = fromJust (lookup name values)

extractLeaf :: AHPTree -> String -> Maybe AHPTree
extractLeaf ahpTree altName = listToMaybe matchingleaves 
	where matchingleaves = filter (\x -> (name x) == altName) leaves
	      leaves = extractLeaves' [] ahpTree

extractLeaves' :: [AHPTree] -> AHPTree -> [AHPTree]
extractLeaves' acc ahpTree =
	case ahpTree of
		AHPTree {} -> concatMap (extractLeaves' acc) (children ahpTree)
		AHPLeaf {} -> [ahpTree] ++ acc

