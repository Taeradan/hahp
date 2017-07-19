-- Test framework
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck         as QC
import           Test.Tasty.SmallCheck         as SC

-- External dependencies
import           Data.List
import           Data.Ord
import           Numeric.LinearAlgebra.HMatrix

-- HAHP
import           HAHP.Algorithm
import           HAHP.Data
import           HAHP.Generator
import           HAHP.Reporting
import           HAHP.Validation.Alternatives
import           HAHP.Validation.Tree
import           HAHP.Validation.Unique

-- Datasets
import           HAHP.Sample.Config1
import           HAHP.Sample.Config2
import           HAHP.Sample.Config3

import           HAHP.Props.Tests
import           HAHP.Unit.Tests
import           Tasty.Sandbox

main = defaultMain tests

tests :: TestTree
--tests = testGroup "Tests" [genericTests, libTests]
tests = testGroup "Tests" [libTests]

-- ----------------------------------------------------------------------------

libTests :: TestTree
libTests = testGroup "Library Tests" [libUnitTests, libScProps, libQcProps]

-- ----------------------------------------------------------------------------
