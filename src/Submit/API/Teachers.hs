{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Submit.API.Teachers
    ( TeachersAPI
    , teachersApplication
    ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Submit.Models
import           Submit.Config
import           Control.Monad.Except
import           Control.Monad.Reader
import           Database.Persist.Postgresql (Entity, selectList)
import           Database.Esqueleto

type TeachersAPI = "teachers" :> Get '[JSON] [Entity Teacher]

teacherServerT :: MonadIO m => ServerT TeachersAPI (AppT m)
teacherServerT = allTeachers

-- | Returns all teachers in the database.
allTeachers :: MonadIO m => AppT m [Entity Teacher]
allTeachers = runDb (selectList [] [])

proxyt :: Proxy TeachersAPI
proxyt = Proxy

teachersServer :: Config -> Server TeachersAPI
teachersServer cfg = Handler $ runReaderT (hoistServer proxyt runApp teacherServerT) cfg

teachersApplication :: Config -> Application
teachersApplication cfg = serve proxyt (teachersServer cfg)