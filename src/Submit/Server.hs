{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Submit.Server
    ( server
    , checkCreds
    , API
    ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text
import           Data.Maybe (listToMaybe)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Control.Monad.Except
import           Control.Monad.Reader
import           Servant             
import           Servant.Server
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan ()
import           Database.Persist.Postgresql (Entity, runSqlPool, selectList)
import           Database.Esqueleto as E
import           GHC.Generics (Generic)

import           Submit.Models
import           Submit.Config
import           Submit.API.Teachers
import           Submit.API.Courses


data Login = Login { username :: Text, password :: Text }
   deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login
instance ToJWT (Entity User)
instance FromJWT (Entity User)


type TotalAPI = TeachersAPI :<|> CoursesAPI

totalProxy :: Proxy TotalAPI
totalProxy = Proxy

type Protected = Auth '[JWT] (Entity User) :> TotalAPI

protected :: Config -> AuthResult (Entity User) -> Server TotalAPI
protected cfg (Authenticated u) = teachersServer cfg :<|> coursesServer cfg u
protected _ _ = throwAll err401

type Unprotected =
    "login"
        :> ReqBody '[JSON] Login
        :> PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                           , Header "Set-Cookie" SetCookie]
                                          NoContent)
     :<|> Raw


unprotected :: Config -> CookieSettings -> JWTSettings -> Server Unprotected
unprotected cfg cs jwts = checkCreds cfg cs jwts :<|> serveDirectoryFileServer "assets"


type API = (Auth '[JWT] (Entity User) :> TotalAPI) :<|> Unprotected


server :: Config -> CookieSettings -> JWTSettings -> Server API
server cfg cs jwts = protected cfg :<|> unprotected cfg cs jwts



-- | 
checkCreds :: Config
           -> CookieSettings
           -> JWTSettings
           -> Login
           -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                                , Header "Set-Cookie" SetCookie]
                               NoContent)
checkCreds cfg cookieSettings jwtSettings (Login username password) = do
    us <- loginHandler username password cfg
    -- Nothing for empty list and Just . head for non-empty lists
    let usr = listToMaybe us
    case usr of
        -- 401 if failed login
        Nothing                -> throwError err401
        Just u@(Entity uid ue) -> do
            mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings u
            -- set cookies
            case mApplyCookies of
                Nothing           -> throwError err401
                Just applyCookies -> return $ applyCookies NoContent


-- | Builds a Handler out of getLogin
loginHandler :: Text -> Text -> Config -> Handler [Entity User]
loginHandler u p cfg = Handler $ (runReaderT $ runDb $ getLogin u p) cfg

-- | Retrieve Users from usernames and passwords
getLogin :: MonadIO m => Text -> Text -> SqlPersistT m [Entity User]
getLogin username password = E.select $
        from $ \u -> do
        where_ ((u ^. UserUsername E.==. val username) E.&&. (u ^. UserPassword E.==. val password))
        return u