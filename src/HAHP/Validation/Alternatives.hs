module HAHP.Validation.Alternatives where

import           Control.Parallel.Strategies
import           Data.Maybe
import           HAHP.Data
import           HAHP.Validation.Unique

validateAlternatives :: AHPTree
                     -> [Alternative]
                     -> [AlternativesError]
validateAlternatives ahpTree alts = validate' ahpTree alts testsList

validate' :: AHPTree
          -> [Alternative]
          -> [AHPTree -> [Alternative] -> Maybe AlternativesError]
          -> [AlternativesError]
validate' ahpTree alts checks = catMaybes $ parMap rseq (\check -> check ahpTree alts) checks

testsList :: [AHPTree -> [Alternative] -> Maybe AlternativesError]
testsList = [ noAlternativesTest
            , alternativesUnicityTest
            ]

noAlternativesTest :: AHPTree
                   -> [Alternative]
                   -> Maybe AlternativesError
noAlternativesTest _ alts =
    if not . null $ alts
       then Nothing
       else Just NoAlternativesError

alternativesUnicityTest :: AHPTree
                        -> [Alternative]
                        -> Maybe AlternativesError
alternativesUnicityTest _ alts =
    if null repeatedAlternativesNames
       then Nothing
       else Just AlternativesUnicityError {repeatedAlternativesNames = repeatedAlternativesNames}
  where repeatedAlternativesNames = repeated . map altName $ alts
