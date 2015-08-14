module Main where

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
    putStrLn "|"
    putStrLn "|-"
    putStrLn "--- Fin"

-- |Random Index estimation function taken from : 
-- "Consistency in the AHP : A new approach"
-- José Antonio ALONSO and Teresa LAMATA,
-- IJUFKBS 2006
randomIndex :: Double -> Double
randomIndex matrixSize = ((0.00149) * (matrixSize^3)) + ((-0.05121) * (matrixSize^2)) + (0.59150 * matrixSize) - 0.79124
