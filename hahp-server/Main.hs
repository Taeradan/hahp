{-# LANGUAGE TypeOperators              #-}

module Main where

import           Data.Aeson.Encode.Pretty      (encodePretty)
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Network.Wai.Handler.Warp
import           Servant

import qualified AHP
import qualified Swagger

-- | Derived from https://github.com/haskell-servant/servant-swagger/blob/master/example/server/Main.hs

-- | Combined API of a Ahp service with Swagger documentation.
type API = Swagger.API :<|> AHP.API

-- | API implementation
server :: Server API
server = Swagger.server :<|> AHP.server

-- | Output generated @swagger.json@ file for the @'AhpAPI'@.
writeSwaggerJSON :: IO ()
writeSwaggerJSON = BL8.writeFile "example/swagger.json" (encodePretty Swagger.ahpSwagger)

serverSettings = setPort 8081 . setHost "*4" $ defaultSettings

main :: IO ()
main = do
  putStrLn "Running on port 8081"
  runSettings serverSettings $ serve (Proxy :: Proxy API) server
