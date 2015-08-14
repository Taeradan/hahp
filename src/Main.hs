module Main where

import Algorithm
import Configuration
import SampleAHPConfig

main = do
    putStrLn "---"
    putStrLn "|- Début du programme"
    putStrLn "|"
    putStrLn "Valeurs de Random Index selon Alonso & Lamata 2006 :"
    let valeursRI = map randomIndex [1..15]
    print valeursRI
    putStrLn "Exemple de configuration :"
    print sampleAHPConfig
    putStrLn "La configuration est elle valide ?"
    if (isAHPConfigurationValid sampleAHPConfig)
        then putStrLn "configuration correcte"
        else putStrLn "configuration invalide"
    putStrLn "|"
    putStrLn "|-"
    putStrLn "--- Fin"

