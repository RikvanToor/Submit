{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Submit.Server
    ( app
    ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Control.Monad.Except
import           Control.Monad.Reader
import           Servant             
import           Servant.Server
import           Database.Persist.Postgresql (Entity, runSqlPool, selectList)

import           Submit.Models
import           Submit.Config
import           Submit.API.Teachers

app :: Config -> Application
app = teachersApplication