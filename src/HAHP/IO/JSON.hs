module HAHP.IO.JSON where

import           Data.Aeson
import           GHC.Generics
import           HAHP.Data
import           HAHP.Data.Core
import           Numeric.LinearAlgebra.HMatrix

instance ToJSON Alternative

instance FromJSON Alternative

instance ToJSON AHPTree

instance FromJSON AHPTree

instance ToJSON (Matrix Double) where
    toJSON m = object [ "matrix" .= toLists m]

instance FromJSON (Matrix Double) where
    parseJSON (Object v) = fromLists <$> (v .: "matrix")
