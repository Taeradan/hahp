module HAHP.Validation.Alternatives where

import           Control.Parallel.Strategies
import           Data.Map                    (notMember)
import           Data.Maybe
import           HAHP.Data
import           HAHP.Validation.Unique

validateAlternatives :: AHPDataSet
                     -> [AlternativesError]
validateAlternatives dataSet = validate' dataSet testsList

validate' :: AHPDataSet
          -> [AHPDataSet -> Maybe AlternativesError]
          -> [AlternativesError]
validate' dataSet checks = catMaybes $ parMap rseq (\check -> check dataSet) checks

testsList :: [AHPDataSet -> Maybe AlternativesError]
testsList = [ noAlternativesTest
            , alternativesUnicityTest
            , indicatorsValuesExistenceTest
            ]

noAlternativesTest :: AHPDataSet
                   -> Maybe AlternativesError
noAlternativesTest (_, alts) =
    if not . null $ alts
       then Nothing
       else Just NoAlternativesError

alternativesUnicityTest :: AHPDataSet
                        -> Maybe AlternativesError
alternativesUnicityTest (_, alts) =
    if null repeatedAlternativesNames
       then Nothing
       else Just AlternativesUnicityError {repeatedAlternativesNames = repeatedAlternativesNames}
  where repeatedAlternativesNames = repeated . map altName $ alts

indicatorsValuesExistenceTest :: AHPDataSet
                              -> Maybe AlternativesError
indicatorsValuesExistenceTest (ahpTree, alts) =
    if null errors
       then Nothing
       else Just IndicatorsValuesExistenceError { indValuesErrors = errors}
  where indicatorsNames = map name (getTreeLeaves ahpTree)
        errors = [ (alt, ind)
                 | alt <- alts
                 , ind <- indicatorsNames
                 , notMember ind . indValues $ alt
                 ]
