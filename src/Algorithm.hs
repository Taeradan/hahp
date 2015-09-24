module Algorithm where

import           Algorithm.Consistency
import           Algorithm.PriorityVector
import           Algorithm.Validation
import           Configuration

initAHP :: AHPTree -> (AHPTree, Bool)
initAHP ahpTree = (newAHPTree, isTreeValid)
    where isTreeValid = isAHPTreeValid newAHPTree
          newAHPTree = computeTreePriorityVectors . computeTreeConsistencies $ ahpTree
