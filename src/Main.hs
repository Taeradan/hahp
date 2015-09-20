module Main where

import Algorithm
import Configuration
import SampleAHPConfig
import Data.Time
import System.Environment


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
    putStrLn $ showConfigurationSummary sampleAHPConfig
    putStrLn $ showConfigurationSummary sampleAHPConfig2

showConfigurationSummary :: AHPTree -> String
showConfigurationSummary ahpTree = concat
    [ "# Configuration \"" ++ name ahpTree ++ "\"\n"
    , "\n"
    , "## Aperçu de la configuration\n"
    , "\n"
    , showAhpTree ahpTree ++ "\n"
    , "\n"
    , "## La configuration est elle valide ?\n"
    , "\n"
    , "critère de cohérence = " ++ show (matrixConsistency (preferenceMatrix ahpTree)) ++ "\n"
    , if isAHPTreeValid ahpTree
        then "-> configuration correcte\n"
        else "-> configuration invalide\n"
    , "\n"
    ]
