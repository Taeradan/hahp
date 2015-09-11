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
    putStrLn "* Valeurs de Random Index selon Alonso & Lamata 2006"
    putStrLn ""
    let valeursRI = map randomIndex [1..15]
    print valeursRI
    putStrLn ""
    putStrLn "Exemple de configuration :"
    print sampleAHPConfig
    putStrLn "La configuration est elle valide ?"
    if isAHPTreeValid sampleAHPConfig
        then putStrLn "configuration correcte"
        else putStrLn "configuration invalide"
    putStrLn "|"
    putStrLn "Exemple de configuration Cortex :"
    print sampleAHPConfig2
    putStrLn "La configuration est elle valide ?"
    if isAHPTreeValid sampleAHPConfig2
        then putStrLn "configuration correcte"
        else putStrLn "configuration invalide"
    putStrLn "|"
    putStrLn "|-"
    putStrLn "--- Fin"

