{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module AHP (API, server) where

import           Servant

import           HAHP.Data.Core
import           HAHP.Sample.CarChoice
import           HAHP.Sample.Config1
import           HAHP.Sample.Config2
import           HAHP.Sample.Config3
import           HAHP.Sample.LeaderChoice

-- | API of a Ahp service.
type API = "ahp" :> "tree"  :>  Get '[JSON] AHPTree
      :<|> "ahp" :> "trees" :>  Get '[JSON] [AHPTree]

-- | API implementation
server :: Server API
server = getTree :<|> getTrees


getTree = return leaderChoiceTree

getTrees = return [ sampleAHPConfig1
                  , sampleAHPConfig2
                  , sampleAHPConfig3
                  , carChoiceTree
                  , leaderChoiceTree
                  ]
