{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}

module Submit.API.Courses 
    ( CoursesAPI
    , coursesApplication
    ) where

import           Data.Aeson
import           Servant
import           Submit.Models
import           Submit.Config
import           Submit.API.Teachers
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Except
import           Database.Persist.Postgresql (Entity, selectList)
import           Database.Esqueleto as E
import           Data.Text
import           Data.Maybe
import           GHC.Generics (Generic)

data CourseInfo = CourseInfo
        { courseInfoCourseCode  :: Text
        , courseInfoDescription :: Text
        , courseInfoName        :: Text
        , courseInfoTeachers    :: [TeacherInfo]
        -- , courseInfoAssistants  :: [StudentInfo]
        } deriving (Generic, Show)

instance ToJSON   CourseInfo
instance FromJSON CourseInfo

toCourseInfo :: (Entity Course, [TeacherInfo]) -> CourseInfo
toCourseInfo (Entity cid c, ts) = CourseInfo (courseCoursecode c) (courseDescription c) (courseCoursename c) ts

-- host/courses/AFP returns data of course with coursecode "AFP"
type CoursesAPI = "courses" :> Capture "coursecode" Text :> Get '[JSON] (Maybe CourseInfo)



coursesServerT :: MonadIO m => ServerT CoursesAPI (AppT m)
coursesServerT = runDb . getCourse

proxyc :: Proxy CoursesAPI
proxyc = Proxy

coursesServer :: Config -> Server CoursesAPI
coursesServer cfg t = Handler $ runReaderT 
    (hoistServer proxyc runApp coursesServerT $ t) 
    cfg

coursesApplication :: Config -> Application
coursesApplication cfg = serve proxyc (coursesServer cfg)



getCourseTeachers :: MonadIO m => (Entity Course) -> SqlPersistT m [TeacherInfo]
getCourseTeachers (Entity cid _) = do
    s <- E.select $
              from $ \(tc,u,t) -> do
              where_ ((tc ^. TeachesCourseid E.==. val cid) E.&&. (tc ^. TeachesTeacherid E.==. t ^. TeacherId) E.&&. (t ^. TeacherUserid E.==. u ^. UserId))
              return (u, t)
    return $ fmap toTeacherInfo s

courseToCourseInfoQuery :: MonadIO m => Entity Course -> SqlPersistT m (Maybe CourseInfo)
courseToCourseInfoQuery c = do
    t <- getCourseTeachers c
    return $ Just $ toCourseInfo (c,t)

getCourse :: MonadIO m => Text -> SqlPersistT m (Maybe CourseInfo)
getCourse code = do
    s <- E.select $
              from $ \c -> do
              where_ (c ^. CourseCoursecode E.==. val code)
              return c
    let c = listToMaybe s
    maybe (return Nothing) courseToCourseInfoQuery c