module HAHP.IO.JSON where

import           Data.Aeson
import           GHC.Generics
import           HAHP.Data
import           Numeric.LinearAlgebra.HMatrix

instance ToJSON Alternative where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Alternative
    -- default implementation


instance ToJSON AHPTree where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON (Matrix Double) where
    toJSON m = object [ "matrix" .= (toLists $ m)]
