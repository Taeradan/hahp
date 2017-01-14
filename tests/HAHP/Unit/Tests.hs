module HAHP.Unit.Tests where

-- Test framework
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

-- External dependencies
import Data.List
import qualified Data.Map                      as M
import Data.Ord
import Numeric.LinearAlgebra.HMatrix

-- HAHP
import HAHP.Algorithm
import HAHP.Data
import HAHP.Data.Core
import HAHP.Data.Utils
import HAHP.Generator
import HAHP.Reporting
import HAHP.Validation.Alternatives
import HAHP.Validation.Tree
import HAHP.Validation.Unique

-- Datasets
import HAHP.Sample.Config1
import HAHP.Sample.Config2
import HAHP.Sample.Config3

-- ----------------------------------------------------------------------------

libUnitTests = testGroup "Library Unit tests" [ libUnitTestsConfig1, libUnitTestsConfig2, libUnitTestsConfig3]

libUnitTestsConfig1 = testGroup "Config1"
    [ testGroup "Invariants"
        [ testCase "preference matrix is square" $ True @=? (cols $ preferenceMatrix tree) == (rows $ preferenceMatrix tree)
        , testCase "decision tree valid" $ True @=? (null $ validateInputAHPTree tree)
        , testCase "alternatives are valid" $ True @=? (null $ validateAlternatives (tree, alts) )
        , testCase "evaluated decision tree valid" $ True @=? (null $ validateAHPTree tree)
        , testCase "tree are changed" $ False @=? (tree == dynTree)
        , testCase "alts content is unchanged" $ True @=? (sort alts == sort dynAlts)
        , testCase "alts order is changed" $ False @=? (alts == dynAlts)
        , testCase "there is no tree error" $ True @=? (null $ treeErr)
        , testCase "there is no tree error -" $ "" @=? (concatMap showTreeError treeErr)
        , testCase "there is no alt error" $ True @=? (null $ altErr)
        , testCase "there is no alt error -" $ "" @=? (concatMap showAltsError altErr)
        , testCase "there is more alternatives values than indicator count" $ (altValuesCount $ head alts) @=? getIndicatorRecursiveCount tree
        , testCase "each alternatives have same values count" $ minValuesCount alts @=? maxValuesCount alts
        ]
    , testGroup "Static part"
        [ testCase "tree name" $ "Super objective" @=? (name tree)
        , testCase "preference matrix size" $ 2 @=? (rows $ preferenceMatrix tree)
        , testCase "preference matrix value" $ fromLists [ [1,1], [1,1]] @=? preferenceMatrix tree
        , testCase "indicators count - top level" $ 2 @=? getIndicatorCurrentLevelCount tree
        , testCase "indicators count - total" $ 2 @=? getIndicatorRecursiveCount tree
        , testCase "alternatives count" $ 6 @=? length alts
        ]
    , testGroup "Dynamic part"
        [ testCase "alternatives initial order is knwown" $ [alt1A, alt1B, alt1C, alt1D, alt1E, alt1F] @=? alts
        , testCase "alternatives ranked  order is knwown" $ [alt1D, alt1C, alt1F, alt1B, alt1A, alt1E] @=? dynAlts
        ]
    ]
    where tree = initAHP sampleAHPConfig1
          alts = sampleAlternatives1
          ( (dynTree, dynAlts), treeErr, altErr) = simpleAHP (tree, alts)

libUnitTestsConfig2 = testGroup "Config2"
    [ testGroup "Invariants"
        [ testCase "preference matrix is square" $ True @=? (cols $ preferenceMatrix tree) == (rows $ preferenceMatrix tree)
        , testCase "decision tree valid" $ True @=? (null $ validateInputAHPTree tree)
        , testCase "alternatives are valid" $ True @=? (null $ validateAlternatives (tree, alts) )
        , testCase "evaluated decision tree valid" $ True @=? (null $ validateAHPTree tree)
        , testCase "tree are changed" $ False @=? (tree == dynTree)
        , testCase "alts content is unchanged" $ True @=? (sort alts == sort dynAlts)
        , testCase "alts order is changed" $ False @=? (alts == dynAlts)
        , testCase "there is no tree error" $ True @=? (null $ treeErr)
        , testCase "there is no tree error -" $ "" @=? (concatMap showTreeError treeErr)
        , testCase "there is no alt error" $ True @=? (null $ altErr)
        , testCase "there is no alt error -" $ "" @=? (concatMap showAltsError altErr)
        , testCase "there is more alternatives values than indicator count" $ (altValuesCount $ head alts) @=? getIndicatorRecursiveCount tree
        , testCase "each alternatives have same values count" $ minValuesCount alts @=? maxValuesCount alts
        ]
    , testGroup "Static part"
        [ testCase "tree name" $ "Become the world's master, Pinky and the Brain" @=? (name tree)
        , testCase "preference matrix size" $ 3 @=? (rows $ preferenceMatrix tree)
        , testCase "preference matrix value" $ fromLists [ [1,0.25,4], [4,1,9], [0.25,0.1111111111111111,1]] @=? preferenceMatrix tree
        , testCase "indicators count - top level" $ 2 @=? getIndicatorCurrentLevelCount tree
        , testCase "indicators count - total" $ 5 @=? getIndicatorRecursiveCount tree
        , testCase "alternatives count" $ 4 @=? length alts
        ]
    , testGroup "Dynamic part"
        [ testCase "alternatives initial order is knwown" $ [alt2John, alt2David, alt2Marc, alt2Steve] @=? alts
        , testCase "alternatives ranked  order is knwown" $ [alt2Marc, alt2David, alt2John, alt2Steve] @=? dynAlts
        ]
    ]
    where tree = initAHP sampleAHPConfig2
          alts = sampleAlternatives2
          ( (dynTree, dynAlts), treeErr, altErr) = simpleAHP (tree, alts)

libUnitTestsConfig3 = testGroup "Config3"
    [ testGroup "Invariants"
        [ testCase "preference matrix is square" $ True @=? (cols $ preferenceMatrix tree) == (rows $ preferenceMatrix tree)
        , testCase "decision tree valid" $ True @=? (null $ validateInputAHPTree tree)
        , testCase "alternatives are valid" $ True @=? (null $ validateAlternatives (tree, alts) )
        , testCase "evaluated decision tree valid" $ True @=? (null $ validateAHPTree tree)
        , testCase "tree are changed" $ False @=? (tree == dynTree)
        , testCase "alts content is unchanged" $ True @=? (sort alts == sort dynAlts)
        , testCase "alts order is changed" $ False @=? (alts == dynAlts)
        , testCase "there is no tree error" $ True @=? (null $ treeErr)
        , testCase "there is no tree error -" $ "" @=? (concatMap showTreeError treeErr)
        , testCase "there is no alt error" $ True @=? (null $ altErr)
        , testCase "there is no alt error -" $ "" @=? (concatMap showAltsError altErr)
        , testCase "there is more alternatives values than indicator count" $ (altValuesCount $ head alts) @=? getIndicatorRecursiveCount tree
        , testCase "each alternatives have same values count" $ minValuesCount alts @=? maxValuesCount alts
        ]
    , testGroup "Static part"
        [ testCase "tree name" $ "Testing the Priority vectors computation" @=? (name tree)
        , testCase "preference matrix size" $ 4 @=? (rows $ preferenceMatrix tree)
        , testCase "preference matrix value" $ fromLists [ [1,1/5,1,3], [5,1,3,5], [1,1/3,1,3], [1/3,1/5,1/3,1]] @=? preferenceMatrix tree
        , testCase "indicators count - top level" $ 4 @=? getIndicatorCurrentLevelCount tree
        , testCase "indicators count - total" $ 4 @=? getIndicatorRecursiveCount tree
        , testCase "alternatives count" $ 3 @=? length alts
        ]
    , testGroup "Dynamic part"
        [ testCase "alternatives initial order is knwown" $ [alt3A, alt3B, alt3C] @=? alts
        , testCase "alternatives ranked  order is knwown" $ [alt3B, alt3A, alt3C] @=? dynAlts
        ]
    ]
    where tree = initAHP sampleAHPConfig3
          alts = sampleAlternatives3
          ( (dynTree, dynAlts), treeErr, altErr) = simpleAHP (tree, alts)

-- ----------------------------------------------------------------------------

altValuesCount :: Alternative -> Int
altValuesCount alt = length . M.toList . indValues $ alt

minValuesCount :: [Alternative] -> Int
minValuesCount alts = minimum $ map altValuesCount alts

maxValuesCount :: [Alternative] -> Int
maxValuesCount alts = maximum $ map altValuesCount alts
--
