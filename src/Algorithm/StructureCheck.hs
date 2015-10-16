module Algorithm.StructureCheck where

import Configuration

isTreeStructureValid :: AHPTree -> Bool
isTreeStructureValid = null . checkTreeStructure

checkTreeStructure :: AHPTree -> [StructureError]
checkTreeStructure = checkTreeStructure' []

-- TODO : implement, issue 6
checkTreeStructure' :: [StructureError] -> AHPTree -> [StructureError]
checkTreeStructure' prevErrors ahpTree = prevErrors ++ newErrors
    where newErrors = []
