{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module AHP (API, server) where

import           Data.List
import           Servant

import           HAHP.Data.Core
import           HAHP.Sample.CarChoice
import           HAHP.Sample.Config1
import           HAHP.Sample.Config2
import           HAHP.Sample.Config3
import           HAHP.Sample.LeaderChoice

-- | API of a Ahp service.
type API = "ahp" :> "tree"  :> Capture "name" String :>  Get '[JSON] AHPTree
      :<|> "ahp" :> "trees" :>  Get '[JSON] [AHPTree]

-- | API implementation
server :: Server API
server = getTree :<|> getTrees

getTree :: String -> Handler AHPTree
getTree x = case findTree of
    (Just tree) -> return tree
    Nothing -> throwError $ err404 { errReasonPhrase = "No AHP tree found for the name \"" ++ x ++ "\""}
  where findTree = find (\ tree -> name tree == x) $ exampleTrees

getTrees :: Handler [AHPTree]
getTrees = return exampleTrees

exampleTrees :: [AHPTree]
exampleTrees = [ sampleAHPConfig1
                  , sampleAHPConfig2
                  , sampleAHPConfig3
                  , carChoiceTree
                  , leaderChoiceTree
                  ]
