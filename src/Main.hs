module Main where

import           Algorithm
import           Configuration
import           Data.Time
import           SampleAHPConfig
import           System.Environment

main :: IO ()
main = do
    time <- getCurrentTime
    putStrLn "% Test de la librairie HAHP"
    putStrLn "% JP P, Y D"
    putStrLn $ "% " ++ showGregorian(utctDay time)
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

showConfigurationSummary :: (AHPTree, Bool) -> String
showConfigurationSummary (ahpTree, validation) = concat
    [ "# Configuration \"" ++ name ahpTree ++ "\"\n"
    , "\n"
    , "## Aperçu de la configuration\n"
    , "\n"
    , showAhpTree ahpTree ++ "\n"
    , "\n"
    , "## La configuration est elle valide ?\n"
    , "\n"
    , if validation
        then "-> configuration correcte\n"
        else "-> configuration invalide\n"
    ]
