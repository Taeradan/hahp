module Algorithm where

import           Algorithm.Consistency
import           Algorithm.PriorityVector
import           Algorithm.Ranking
import           Algorithm.Validation
import           Configuration
import           Data.List
import           Data.Maybe
import           Numeric.LinearAlgebra.HMatrix

initAHP :: AHPTree -> (AHPTree, Bool)
initAHP ahpTree = (newAHPTree, isTreeValid)
    where isTreeValid = isAHPTreeValid newAHPTree
          newAHPTree = computeTreePriorityVectors . computeTreeConsistencies $ ahpTree

rankAlternatives :: [Alternative] -> AHPTree -> [Alternative]
rankAlternatives alts ahpTree = map fst sortedAltsRanks
    where ranks = concat . toLists . fromJust $ alternativesPriority rankedAhpTree
          altsRanks = zip alts ranks
          sortedAltsRanks = sortOn snd altsRanks
          rankedAhpTree = computeTreeAlternativesPriorities alts ahpTree

