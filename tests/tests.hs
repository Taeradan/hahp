-- Test framework
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

-- External dependencies
import Data.List
import Data.Ord
import Numeric.LinearAlgebra.HMatrix

-- HAHP
import HAHP.Algorithm
import HAHP.Data
import HAHP.Generator
import HAHP.Reporting
import HAHP.Validation.Alternatives
import HAHP.Validation.Tree
import HAHP.Validation.Unique

-- Datasets
import HAHP.Sample.Config1
import HAHP.Sample.Config2
import HAHP.Sample.Config3

import HAHP.HUnit.Test
import Tasty.Sandbox

main = defaultMain tests

tests :: TestTree
--tests = testGroup "Tests" [genericTests, libTests]
tests = testGroup "Tests" [libTests]

-- ----------------------------------------------------------------------------

libTests :: TestTree
libTests = testGroup "Library Tests" [libUnitTests, libScProps, libQcProps]

libScProps = testGroup "(checked by SmallCheck)"
    [
       SC.testProperty "generated matrix size is good -  rows & cols" $ \size -> matrixSizeGood (size :: Int)
    ,  SC.testProperty "generated matrix size is good - cols" $ \size -> matrixIsSquare (size :: Int)
      --SC.testProperty "Squared numbers are positives or null" $ \size -> (size*size :: Integer) >= 0
    ]

libQcProps = testGroup "(checked by QuickCheck)"
    [
      --QC.testProperty "Squared numbers are positives or null" $ \size -> (size*size :: Integer) >= 0
       QC.testProperty "generated matrix size is good -  rows & cols" $ \size -> matrixSizeGood (size :: Int)
    ,  QC.testProperty "generated matrix size is good - cols" $ \size -> matrixIsSquare (size :: Int)
    ]
-- ----------------------------------------------------------------------------

matrixSizeGood :: Int -> Bool
matrixSizeGood size = (rows $ matrix) == (cols $ matrix) &&  (rows $ matrix) == size
    where matrix = generateMatrix size

matrixIsSquare :: Int -> Bool
matrixIsSquare size = (rows $ matrix) == (cols $ matrix)
    where matrix = generateMatrix size

-- ----------------------------------------------------------------------------
