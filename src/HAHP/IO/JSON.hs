module HAHP.IO.JSON where

import           Data.Aeson
import           GHC.Generics
import           HAHP.Data
import           Numeric.LinearAlgebra.HMatrix

instance ToJSON Alternative where
    toEncoding = genericToEncoding defaultOptions

