{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}

module Submit.API.Teachers
    ( TeachersAPI
    , TeacherInfo
    , toTeacherInfo
    , teachersServer
    ) where

import           Data.Aeson
import           Servant
import           Submit.Models
import           Submit.Config
import           Control.Monad.Reader
import           Database.Persist.Postgresql (Entity, selectList)
import           Database.Esqueleto as E
import           Data.Text
import           GHC.Generics (Generic)

-- | Represents a teacher containing the following info: userID, name and office location
data TeacherInfo = TeacherInfo
        { teacherInfoUserId :: UserId -- ^ The teacher's unique userID.
        , teacherInfoName   ::  Text  -- ^ The teacher's name.
        , teacherInfoOffice :: Text   -- ^ The teacher's office.
        } deriving (Generic, Eq, Show)

instance FromJSON TeacherInfo
instance ToJSON   TeacherInfo

-- | Converts a tuple of Persistent User and Teacher entities to TeacherInfo
toTeacherInfo :: (Entity User, Entity Teacher) -> TeacherInfo
toTeacherInfo (Entity uid u, Entity tid t) = TeacherInfo uid (userName u) (teacherOffice t)

-- | host/teachers returns all teachers
type TeachersAPI = "teachers" :> Get '[JSON] [TeacherInfo]

teacherServerT :: MonadIO m => ServerT TeachersAPI (AppT m)
teacherServerT = runDb allTeachers


-- | Returns all teachers
allTeachers :: MonadIO m => SqlPersistT m [TeacherInfo]
allTeachers = do
  s <- E.select $
              from $ \(u,t) -> do
              where_ ((u ^. UserId) E.==. (t ^. TeacherUserid))
              return (u,t)
  return $ fmap toTeacherInfo s
  

proxyt :: Proxy TeachersAPI
proxyt = Proxy

teachersServer :: Config -> Server TeachersAPI
teachersServer cfg = Handler $ runReaderT (hoistServer proxyt runApp teacherServerT) cfg