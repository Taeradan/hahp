module Main where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy.Char8 (unpack)
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

    time <- getCurrentTime
    putStrLn $ reportHeader title author time
    putStrLn ""
    mapM_ (putStrLn . simpleAHPSummary) inputDataSets
    writeFile "alts.json" $ unpack . encode $ firstAlternatives
    writeFile "alts-pretty.json" $ unpack . encodePretty $ firstAlternatives

simpleAHPSummary :: AHPDataSet -> String
simpleAHPSummary dataSet = simpleSummary . simpleAHP $ dataSet
