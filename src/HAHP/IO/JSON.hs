module HAHP.IO.JSON where

import           Data.Aeson
import           GHC.Generics
import           HAHP.Data
import           Numeric.LinearAlgebra.HMatrix

instance ToJSON Alternative where
    -- default implementation

instance FromJSON Alternative
    -- default implementation

instance ToJSON AHPTree where
    -- default implementation

instance FromJSON AHPTree where
--    fromJSON (Object v) = AHPTree

instance ToJSON (Matrix Double) where
    toJSON m = object [ "matrix" .= toLists m]

instance FromJSON (Matrix Double) where
    parseJSON (Object v) = fromLists <$> (v .: "matrix")
