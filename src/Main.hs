module Main where

import           Algorithm
import           Algorithm.Consistency
import           Algorithm.Ranking
import           Data.Time
import           Reporting
import           SampleAHP.Config1
import           SampleAHP.Config2
import           SampleAHP.Config3

main :: IO ()
main = do
    time <- getCurrentTime
    let title = "Test de la librairie HAHP"
        author = "JP P, Y D"
    putStrLn $ reportHeader title author time
    putStrLn ""
    putStrLn "# Valeurs de Random Index selon Alonso & Lamata 2006"
    putStrLn ""
    let valeursRI = map randomIndexCalculated [1..15]
    print valeursRI
    putStrLn ""
    let configs = [sampleAHPConfig, sampleAHPConfig2, sampleAHPConfig3]
    --mapM_ (putStrLn . showConfigurationSummary . initAHP) configs
    let (completeTree, ranking, validation) = simpleAHP sampleAHPConfig3 sampleAlternatives3
    putStrLn . showConfigurationSummary $ (completeTree, validation)
    putStr $ showAlternatives ranking
