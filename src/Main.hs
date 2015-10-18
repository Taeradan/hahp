module Main where

import           Data.Time
import           HAHP.Algorithm
import           HAHP.Data
import           HAHP.Reporting
import           HAHP.Sample.Config1
import           HAHP.Sample.Config2
import           HAHP.Sample.Config3

main :: IO ()
main = do
    let title = "Test de la librairie HAHP"
        author = "JP P, Y D"
        configs = [sampleAHPConfig1, sampleAHPConfig2, sampleAHPConfig3]
        alternatives = [sampleAlternatives1, sampleAlternatives2, sampleAlternatives3]
        sampleDataSet = zip configs alternatives

    time <- getCurrentTime
    putStrLn $ reportHeader title author time
    putStrLn ""
    mapM_ (putStrLn . simpleAHPSummary) sampleDataSet
    --mapM_ (putStrLn . showConfigurationSummary . initAHP) configs
    --putStrLn . showConfigurationSummary $ (completeTree, validation)
    --putStr $ showAlternatives ranking

simpleAHPSummary :: (AHPTree, [Alternative]) -> String
simpleAHPSummary (ahpTree, alts) = simpleSummary $ simpleAHP ahpTree alts
