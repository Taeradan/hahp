module Main where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Maybe
import           Data.Time
import           HAHP.Algorithm
import           HAHP.Data
import           HAHP.Generator
import           HAHP.IO.JSON
import           HAHP.Reporting
import           HAHP.Sample.Config1
import           HAHP.Sample.Config2
import           HAHP.Sample.Config3
import           HAHP.Sample.CarChoice
import           HAHP.Sample.LeaderChoice
import           HAHP.Sample.SquareMatrixError

main :: IO ()
main = do
    let title = "HAHP library testing"
        author = "Jean-Pierre Prunaret, Yves Dubromelle"
        inputDataSets = [ --(sampleAHPConfig1, sampleAlternatives1)
                        --, (sampleAHPConfig2, sampleAlternatives2)
                        --, (sampleAHPConfig3, sampleAlternatives3)
                        --, (smeConfig, smeAlternatives)
                         (leaderChoiceTree, leaderChoiceAlternatives)
                        , (carChoiceTree, carChoiceAlternatives)
                        --, generateDataSet $ GeneratorParameters True 3 3 100
                        --, generateDataSet $ GeneratorParameters False 3 3 100
                        ]
        firstAlternatives = snd . head $ inputDataSets
        firstTree = fst . head $ inputDataSets

-- processing of the input data sets
    time <- getCurrentTime
    putStrLn $ reportHeader title author time
    putStrLn ""
    mapM_ (putStrLn . simpleAHPSummary) inputDataSets

-- JSON export of the first dataset
    B.writeFile "alts.json" $ encode firstAlternatives
    B.writeFile "alts-pretty.json" $ encodePretty firstAlternatives
    B.writeFile "tree-pretty.json" $ encodePretty firstTree

-- JSON import of sample alternatives
    leaderAltsJson <- B.readFile "leader-alts.json"
    let leaderAlts = (eitherDecode leaderAltsJson) :: Either String [Alternative]
    putStrLn $ either show showAlternatives leaderAlts

-- JSON import of sample AHP tree
    leaderTreeJson <- B.readFile "leader-tree.json"
    let leaderTree = (eitherDecode leaderTreeJson) :: Either String AHPTree
    putStrLn $ either show showAhpTree leaderTree

simpleAHPSummary :: AHPDataSet -> String
simpleAHPSummary dataSet = simpleSummary . simpleAHP $ dataSet
