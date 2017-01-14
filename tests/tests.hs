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
import HAHP.Data
import HAHP.Validation.Alternatives
import HAHP.Validation.Tree
import HAHP.Validation.Unique

-- Datasets
import HAHP.Sample.Config1
import HAHP.Sample.Config2
import HAHP.Sample.Config3

main = defaultMain tests

tests :: TestTree
--tests = testGroup "Tests" [genericTests, libTests]
tests = testGroup "Tests" [libTests]

-- ----------------------------------------------------------------------------

libTests :: TestTree
libTests = testGroup "Library Tests" [libUnitTests, libScProps, libQcProps]

libScProps = testGroup "(checked by SmallCheck)"
    [
    ]

libQcProps = testGroup "(checked by QuickCheck)"
    [
    ]

libUnitTests = testGroup "Library Unit tests" [ libUnitTestsConfig1, libUnitTestsConfig2, libUnitTestsConfig3]

libUnitTestsConfig1 = testGroup "Config1"
    [ testCase "tree name" $ "Super objective" @=? (name tree)
    , testCase "preference matrix size" $ 2 @=? (rows $ preferenceMatrix tree)
    , testCase "preference matrix is square" $ True @=? (cols $ preferenceMatrix tree) == (rows $ preferenceMatrix tree)
    , testCase "preference matrix value" $ fromLists [ [1,1], [1,1]] @=? preferenceMatrix tree
    , testCase "indicators count - top level" $ 2 @=? (length $ children tree)
    , testCase "indicators count - total" $ 2 @=? (length $ children tree)
    , testCase "decision tree valid" $ True @=? (null $ validateInputAHPTree tree)
    , testCase "alternatives count" $ 6 @=? length alts
    , testCase "alternatives are valid" $ True @=? (null $ validateAlternatives (tree, alts) )
    ]
    where tree = sampleAHPConfig1
          alts = sampleAlternatives1

libUnitTestsConfig2 = testGroup "Config2"
    [ testCase "tree name" $ "Become the world's master, Pinky and the Brain" @=? (name tree)
    , testCase "preference matrix size" $ 3 @=? (rows $ preferenceMatrix tree)
    , testCase "preference matrix is square" $ True @=? (cols $ preferenceMatrix tree) == (rows $ preferenceMatrix tree)
    , testCase "preference matrix value" $ fromLists [ [1,0.25,4], [4,1,9], [0.25,0.1111111111111111,1]] @=? preferenceMatrix tree
    , testCase "indicators count - top level" $ 2 @=? (length $ children tree)
    , testCase "indicators count - total" $ 5 @=? (length $ children tree)
    , testCase "decision tree valid" $ True @=? (null $ validateInputAHPTree tree)
    , testCase "alternatives count" $ 4 @=? length alts
    , testCase "alternatives are valid" $ True @=? (null $ validateAlternatives (tree, alts) )
    ]
    where tree = sampleAHPConfig2
          alts = sampleAlternatives2

libUnitTestsConfig3 = testGroup "Config3"
    [ testCase "tree name" $ "Testing the Priority vectors computation" @=? (name tree)
    , testCase "preference matrix size" $ 4 @=? (rows $ preferenceMatrix tree)
    , testCase "preference matrix is square" $ True @=? (cols $ preferenceMatrix tree) == (rows $ preferenceMatrix tree)
    , testCase "preference matrix value" $ fromLists [ [1,1/5,1,3], [5,1,3,5], [1,1/3,1,3], [1/3,1/5,1/3,1]] @=? preferenceMatrix tree
    , testCase "indicators count - top level" $ 4 @=? (length $ children tree)
    , testCase "indicators count - total" $ 4 @=? (length $ children tree)
    , testCase "decision tree valid" $ True @=? (null $ validateInputAHPTree tree)
    , testCase "alternatives count" $ 3 @=? length alts
    , testCase "alternatives are valid" $ True @=? (null $ validateAlternatives (tree, alts) )
    ]
    where tree = sampleAHPConfig3
          alts = sampleAlternatives3

-- ----------------------------------------------------------------------------

genericTests :: TestTree
genericTests = testGroup "Generic Tests" [genericProperties, genericUnitTests]

genericProperties :: TestTree
genericProperties = testGroup "Generic Properties" [genericScProps, genericQcProps]

genericScProps = testGroup "Generic (checked by SmallCheck)"
  [ SC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , SC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , SC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
  ]

genericQcProps = testGroup "Generic (checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , QC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  ]

genericUnitTests = testGroup "Generic Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= LT
  ]
-- ----------------------------------------------------------------------------

