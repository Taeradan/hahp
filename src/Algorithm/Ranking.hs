module Algorithm.Ranking where

import           Algorithm.PriorityVector
import           Configuration
import           Data.List
import           Data.Maybe
import           Debug.Trace
import           Numeric.LinearAlgebra.HMatrix

computeTreeAlternativesPriorities :: [Alternative] -> AHPTree -> AHPTree
computeTreeAlternativesPriorities alts ahpTree = 
    case ahpTree of
        (AHPTree _ _ _ _ _ children) -> ahpTreeWithChildren
            { alternativesPriority = Just . (trace $ "###### agregatePriorities de " ++ name ahpTree) .agregatePriorities $ ahpTree
            }
            where ahpTreeWithChildren = ahpTree {
                children = map (computeTreeAlternativesPriorities alts) children 
                }
--        AHPLeaf {} -> ahpTree
        AHPLeaf {name = name} -> ahpTree
            { alternativesPriority = Just $ computeAlternativesPriority alts name
            }

agregatePriorities :: AHPTree -> PriorityVector
agregatePriorities ahpTree = catChildVectors <> childPriorities
    where childVectors = map (fromJust . alternativesPriority) (children ahpTree)
          catChildVectors = foldl1 (|||) childVectors
          childPriorities = fromJust . childrenPriority $ ahpTree

computeAlternativesPriority :: [Alternative] -> IndicatorName -> PriorityVector
computeAlternativesPriority alts name = (1 >< (length alts) )[1, 1..]
