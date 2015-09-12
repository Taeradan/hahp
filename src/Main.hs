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
    printConfigurationSummary sampleAHPConfig
    printConfigurationSummary sampleAHPConfig2

printConfigurationSummary :: AHPTree -> IO ()
printConfigurationSummary ahpTree = do
    putStrLn "# Exemple de configuration"
    putStrLn ""
    putStrLn "## Aperçu de la configuration"
    putStrLn ""
    putStrLn "\\begin{verbatim}"
    print ahpTree
    putStrLn "\\end{verbatim}"
    putStrLn ""
    putStrLn "## La configuration est elle valide ?"
    putStrLn ""
    putStrLn $ "critère de cohérence = " ++ show (matrixConsistency (preferenceMatrix ahpTree))
    if isAHPTreeValid ahpTree
        then putStrLn "configuration correcte"
        else putStrLn "configuration invalide"
    putStrLn ""
