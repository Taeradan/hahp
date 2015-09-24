module Main where

import           Algorithm
import           Configuration
import           Data.Time
import           Reporting
import           SampleAHPConfig
import           System.Environment

main :: IO ()
main = do
    time <- getCurrentTime
    let title = "Test de la librairie HAHP"
        author = "JP P, Y D"
    putStrLn $ reportHeader title author time
    putStrLn ""
    putStrLn "# Valeurs de Random Index selon Alonso & Lamata 2006"
    putStrLn ""
    let valeursRI = map randomIndex [1..15]
    print valeursRI
    putStrLn ""
    let processedConfig1 = initAHP sampleAHPConfig
    let processedConfig2 = initAHP sampleAHPConfig2
    let processedConfig3 = initAHP sampleAHPConfig3
    putStrLn $ showConfigurationSummary processedConfig1
    putStrLn $ showConfigurationSummary processedConfig2
    putStrLn $ showConfigurationSummary processedConfig3
