{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}

module Submit.Server where

import           Data.Text
import           GHC.Generics (Generic)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Maybe (listToMaybe)
import           Data.Text.Encoding (decodeUtf8)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Control.Monad.Except
import           Control.Monad.Reader
import           Servant             
import           Servant.Server
import           Database.Persist.Postgresql (Entity, runSqlPool, selectList)
import           Database.Esqueleto as E

import           Submit.Models
import           Submit.Config
import           Submit.API.Teachers
import           Submit.API.Courses

type TotalAPI = TeachersAPI :<|> CoursesAPI

totalProxy :: Proxy TotalAPI
totalProxy = Proxy

app :: Config -> Application
app cfg = serve totalProxy (teachersServer cfg :<|> coursesServer cfg)

data UserAuth = UserAuth
    { username  :: Text
    , password  :: Text
    , name      :: Text
    , userid    :: UserId
    , studentid :: Maybe StudentId
    , teacherid :: Maybe TeacherId
    } deriving (Eq, Show, Generic)

instance FromJSON UserAuth
instance ToJSON UserAuth

type AuthAPI = BasicAuth "private" UserAuth :> TotalAPI

authProxy :: Proxy AuthAPI
authProxy = Proxy

authServer :: Config -> Server AuthAPI
authServer cfg ua = (teachersServer cfg :<|> coursesServer cfg)

checkBasicAuth :: Config -> BasicAuthCheck UserAuth
checkBasicAuth = BasicAuthCheck . checkBasicAuth'

checkBasicAuth' :: Config -> BasicAuthData -> IO (BasicAuthResult UserAuth)
checkBasicAuth' cfg (BasicAuthData u p) = do
    x <- test cfg (decodeUtf8 u) (decodeUtf8 p)
    case x of
        Nothing -> return Unauthorized
        (Just ua) -> return $ Authorized ua

test :: MonadIO m => Config -> Text -> Text -> m (Maybe UserAuth)
test cfg u p = (runReaderT $ runDb $ getLoginData u p) cfg


getLoginData :: MonadIO m => Text -> Text -> SqlPersistT m (Maybe UserAuth)
getLoginData username password = do
    s <- E.select $
              from $ \u -> do
              where_ ((u ^. UserUsername E.==. val username) E.&&. (u ^. UserPassword E.==. val password))
              return u
    let u = listToMaybe s
    case u of
        Nothing -> return Nothing
        (Just (Entity uid user)) -> do
            st <- E.select $
                from $ \stu -> do
                    where_ ((stu ^. StudentUserid E.==. val uid))
                    return stu
            let stid = maybe Nothing (Just . entityKey) $ listToMaybe st
            te <- E.select $
                from $ \t -> do
                    where_ ((t ^. TeacherUserid E.==. val uid))
                    return t
            let teid = maybe Nothing (Just . entityKey) $ listToMaybe te
            let ua = UserAuth { username  = userUsername user
                              , password  = userPassword user
                              , name      = userName     user
                              , userid    = uid
                              , studentid = stid
                              , teacherid = teid
                              }
            return $ Just ua