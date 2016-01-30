module HAHP.Validation.Alternatives where

import           Control.Parallel.Strategies
import           Data.Maybe
import           HAHP.Data
import           HAHP.Validation.Unique

validateAlternatives :: [Alternative] -> [AlternativesError]
validateAlternatives alts = validate' alts testsList

validate' :: [Alternative] -> [[Alternative] -> Maybe AlternativesError] -> [AlternativesError]
validate' alts checks = catMaybes $ parMap rseq (\check -> check alts) checks

testsList :: [[Alternative] -> Maybe AlternativesError]
testsList = [ noAlternativesTest
            , alternativesUnicityTest
            ]

noAlternativesTest :: [Alternative] -> Maybe AlternativesError
noAlternativesTest alts =
    if not . null $ alts
       then Nothing
       else Just NoAlternativesError

alternativesUnicityTest :: [Alternative]
                        -> Maybe AlternativesError
alternativesUnicityTest alts =
    if null repeatedAlternativesNames
       then Nothing
       else Just AlternativesUnicityError {repeatedAlternativesNames = repeatedAlternativesNames}
  where repeatedAlternativesNames = repeated . map altName $ alts
