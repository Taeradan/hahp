module HAHP.Data where

import           Data.Map                      (Map)
import           GHC.Generics
import           Numeric.LinearAlgebra.HMatrix

import           HAHP.Data.Core
import           HAHP.Data.Errors
import           HAHP.Data.Utils

-- * Data set macro type

type AHPDataSet = (AHPTree, [Alternative])

data GeneratorParameters = GeneratorParameters { randomSize       :: Bool
                                               , maxTreeLevels    :: Int
                                               , maxLevelChildren :: Int
                                               , maxAlternatives  :: Int
                                               }

