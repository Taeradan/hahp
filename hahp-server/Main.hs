{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

import           HAHP.Data.Core
import           HAHP.IO.JSON
import           HAHP.Sample.Config1
import           HAHP.Sample.Config2
import           HAHP.Sample.Config3
import           HAHP.Sample.CarChoice
import           HAHP.Sample.LeaderChoice

type AhpAPI1 = "ahp" :> "tree" :>  Get '[JSON] AHPTree
          :<|> "ahp" :> "trees" :>  Get '[JSON] [AHPTree]

ahpDataServer :: Server AhpAPI1
ahpDataServer = return leaderChoiceTree
            :<|> return [ sampleAHPConfig1
                        , sampleAHPConfig2
                        , sampleAHPConfig3
                        , carChoiceTree
                        , leaderChoiceTree
                        ]

ahpAPI :: Proxy AhpAPI1
ahpAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve ahpAPI ahpDataServer

main :: IO ()
main = run 8081 app