{-# LANGUAGE DataKinds                  #-}

module Swagger (API, server, ahpSwagger) where

import           Control.Lens
import           Data.Swagger
import           Data.Aeson.Types
import           GHC.Generics
import           Numeric.LinearAlgebra.HMatrix
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI

import           HAHP.Data.Core
import           HAHP.IO.JSON
import           HAHP.Sample.Config1

import qualified AHP

-- | API for serving @swagger.json@.
type API = SwaggerSchemaUI "swagger-ui" "swagger.json"

instance ToSchema AHPTree where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "This is some real AHPTree right here"
    & mapped.schema.example ?~ toJSON sampleAHPConfig1

-- TODO Document the API for Matrix Double
instance Generic (Matrix Double)
instance ToSchema (Matrix Double) where
  declareNamedSchema _ = do
    doubleSchema <- declareSchemaRef (Proxy :: Proxy Double)
    return $ NamedSchema (Just "TODO") $ mempty
      & type_ .~ SwaggerObject

ahpApiProxy :: Proxy AHP.API
ahpApiProxy = Proxy

-- | Swagger spec for Ahp API.
ahpSwagger :: Swagger
ahpSwagger = toSwagger ahpApiProxy
  & info.title   .~ "Ahp API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

server :: Server API
server = swaggerSchemaUIServer ahpSwagger
