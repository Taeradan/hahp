module HAHP.Algorithm.Validation.Alternatives where

import           Control.Parallel.Strategies
import           Data.Maybe
import           HAHP.Data

validateAlternatives :: [Alternative] -> ([Alternative], [AlternativesError])
validateAlternatives alts = validate' alts testsList

validate' :: [Alternative] -> [[Alternative] -> Maybe AlternativesError] -> ([Alternative], [AlternativesError])
validate' alts checks = ( alts
                        , catMaybes $ parMap rseq (\check -> check alts) checks
                        )

testsList :: [[Alternative] -> Maybe AlternativesError]
testsList = [ noAlternativesTest
            ]

noAlternativesTest :: [Alternative] -> Maybe AlternativesError
noAlternativesTest alts =
    if not . null $ alts
       then Nothing
       else Just NoAlternativesError

