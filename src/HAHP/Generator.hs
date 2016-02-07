module HAHP.Generator where

import           Data.Map (empty, singleton, fromList)
import           HAHP.Data
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
generateAHPTree _ = AHPLeaf "Dummy tree" True Nothing

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
