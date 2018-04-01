{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveGeneric     #-}

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
import           Database.Persist
import           Database.Persist.Postgresql (Entity, selectList)
import           Database.Esqueleto as E
import           Data.Text
import           GHC.Generics (Generic)

data ApiTeacher = ApiTeacher
        { apiTeacherUserId :: UserId
        , apiTeacherName   ::  Text
        , apiTeacherOffice :: Text
        } deriving (Generic, Show)

instance FromJSON ApiTeacher
instance ToJSON   ApiTeacher

toApiTeacher :: (Entity User, Entity Teacher) -> ApiTeacher
toApiTeacher (Entity uid u, Entity tid t) = ApiTeacher uid (userName u) (teacherOffice t)

type TeachersAPI = "teachers" :> Get '[JSON] [ApiTeacher]

teacherServerT :: MonadIO m => ServerT TeachersAPI (AppT m)
teacherServerT = runDb altall



-- | Returns all teachers in the database.
allTeachers :: MonadIO m => AppT m [Entity Teacher]
allTeachers = runDb (selectList [] [])

altall :: MonadIO m => SqlPersistT m [ApiTeacher]
altall = do
  s <- E.select $
              from $ \(u,t) -> do
              where_ (u ^. UserId E.==. t ^. TeacherUserid)
              return (u,t)
  return $ fmap toApiTeacher s
  

proxyt :: Proxy TeachersAPI
proxyt = Proxy

teachersServer :: Config -> Server TeachersAPI
teachersServer cfg = Handler $ runReaderT (hoistServer proxyt runApp teacherServerT) cfg

teachersApplication :: Config -> Application
teachersApplication cfg = serve proxyt (teachersServer cfg)