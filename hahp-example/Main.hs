module Main where

import           Data.Time
import           HAHP.Algorithm
import           HAHP.Data
import           HAHP.Reporting
import           HAHP.Sample.Config1
import           HAHP.Sample.Config2
import           HAHP.Sample.Config3
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
                        ]

    time <- getCurrentTime
    putStrLn $ reportHeader title author time
    putStrLn ""
    mapM_ (putStrLn . simpleAHPSummary) inputDataSets

simpleAHPSummary :: (AHPTree, [Alternative]) -> String
simpleAHPSummary (ahpTree, alts) = simpleSummary $ simpleAHP ahpTree alts
