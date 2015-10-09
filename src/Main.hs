module Main where

import           Algorithm
import           Algorithm.Consistency
import           Algorithm.Ranking
import           Configuration
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
    let configs = [sampleAHPConfig1, sampleAHPConfig2, sampleAHPConfig3]
    let alternatives = [sampleAlternatives1, sampleAlternatives2, sampleAlternatives3]
    let sampleDataSet = zip configs alternatives
    mapM_ (putStrLn . simpleAHPSummary) sampleDataSet
    --mapM_ (putStrLn . showConfigurationSummary . initAHP) configs
    --putStrLn . showConfigurationSummary $ (completeTree, validation)
    --putStr $ showAlternatives ranking

simpleAHPSummary :: (AHPTree, [Alternative]) -> String
simpleAHPSummary (ahpTree, alts) = simpleSummary (completeTree, ranking, validation)
	where (completeTree, ranking, validation) = simpleAHP ahpTree alts
