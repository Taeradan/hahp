module HAHP.Generator where

import           Data.List (insert)
import           Data.Map (empty, singleton, fromList)
import           HAHP.Data
import           Numeric.LinearAlgebra.Data (ident, (><))
import           System.IO.Unsafe
import           System.Random

-- * Data set generator

generateDataSet :: GeneratorParameters
                -> AHPDataSet
generateDataSet params = (ahpTree, alternatives)
    where ahpTree = generateAHPTree params
          alternatives = generateAlternatives params ahpTree

-- * AHP tree generator

generateAHPTree :: GeneratorParameters
                ->AHPTree
generateAHPTree params = generateAHPTree' params levels []
    where levels = if randomSize params
                      then unsafeRandomRIO (1, maxTreeLevels params)
                      else maxTreeLevels params

generateAHPTree' :: GeneratorParameters
                 -> Int
                 -> [Int]
                 -> AHPTree
generateAHPTree' params maxlevels parentIndexes = if (length parentIndexes) + 1 >= maxlevels
                                             then AHPLeaf treeName True Nothing
                                             else AHPTree { name = treeName
                                                          , preferenceMatrix = generateMatrix (childNum)
                                                          , consistencyValue = Nothing
                                                          , childrenPriority = Nothing
                                                          , alternativesPriority = Nothing
                                                          , children = children
                                                          }
  where treeName = if null parentIndexes
                      then "Global Objective"
                      else "Node " ++ (concatMap (\x -> show x ++ ".") parentIndexes)
        childNum = if randomSize params
                      then unsafeRandomRIO (1, maxLevelChildren params)
                      else maxLevelChildren params
        children = take childNum $ map (\x -> generateAHPTree' params maxlevels (parentIndexes ++ [x])) [1..]

generateMatrix :: Int
               -> PairwiseMatrix
generateMatrix size = (size><size) $ repeat 1

-- * Alternatives generator

generateAlternatives :: GeneratorParameters
                     -> AHPTree
                     -> [Alternative]
generateAlternatives params ahpTree = take altsNum randomAlts
  where altsNum = if randomSize params
                     then unsafeRandomRIO (1, maxAlternatives params)
                     else maxAlternatives params
        inds = map name . getTreeLeaves $ ahpTree
        randomAlts = map (generateAlternative inds) [1..]

generateAlternative :: [IndicatorName]
                    -> Int
                    -> Alternative
generateAlternative indNames index = Alternative name values
    where name = "Alternative " ++ show index
          values = fromList $ zip indNames randomValues
          randomValues = unsafePerformIO . sequence . replicate (length indNames) . randomRIO $ (1, 100)

-- * Tools

unsafeRandomRIO :: (Random a) => (a, a) -> a
unsafeRandomRIO range = unsafePerformIO . randomRIO $ range
