{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import           Prelude                       ()
import           Prelude.Compat

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Compat
import           Data.Aeson.Encode.Pretty      (encodePretty)
import qualified Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Attoparsec.ByteString
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Data.List
import           Data.Maybe
import           Data.Proxy
import           Data.String.Conversions
import           Data.Swagger
import           Data.Text                     (Text)
import           Data.Time                     (UTCTime (..), fromGregorian)
import           Data.Time.Calendar
import           Data.Typeable                 (Typeable)
import           GHC.Generics
import           Lucid hiding (type_)
import           Network.HTTP.Media            ((//), (/:))
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Numeric.LinearAlgebra.HMatrix
import           Servant
import           Servant.Swagger
import           System.Directory
import           Text.Blaze
import qualified Text.Blaze.Html
import           Text.Blaze.Html.Renderer.Utf8

import           HAHP.Data.Core
import           HAHP.IO.JSON
import           HAHP.Sample.CarChoice
import           HAHP.Sample.Config1
import           HAHP.Sample.Config2
import           HAHP.Sample.Config3
import           HAHP.Sample.LeaderChoice

-- | Derived from https://github.com/haskell-servant/servant-swagger/blob/master/example/server/Main.hs

-- | The API of a Ahp service.
type AhpAPI = "ahp" :> "tree" :>  Get '[JSON] AHPTree
          :<|> "ahp" :> "trees" :>  Get '[JSON] [AHPTree]

-- | API for serving @swagger.json@.
type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

-- | Combined API of a Ahp service with Swagger documentation.
type API = SwaggerAPI :<|> AhpAPI

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

ahpAPI :: Proxy AhpAPI
ahpAPI = Proxy

-- | Swagger spec for Ahp API.
ahpSwagger :: Swagger
ahpSwagger = toSwagger ahpAPI
  & info.title   .~ "Ahp API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

-- | Combined server of a Ahp service with Swagger documentation.
server :: Server API
server = return ahpSwagger :<|> return leaderChoiceTree
            :<|> return [ sampleAHPConfig1
                        , sampleAHPConfig2
                        , sampleAHPConfig3
                        , carChoiceTree
                        , leaderChoiceTree
                        ]

-- | Output generated @swagger.json@ file for the @'AhpAPI'@.
writeSwaggerJSON :: IO ()
writeSwaggerJSON = BL8.writeFile "example/swagger.json" (encodePretty ahpSwagger)

serverSettings = setPort 8081 . setHost "*4" $ defaultSettings

main :: IO ()
main = do
  putStrLn "Running on port 8081"
  runSettings serverSettings $ serve (Proxy :: Proxy API) server
