{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}

module Submit.API.Courses 
    ( CoursesAPI
    , coursesServer
    ) where

import           Data.Aeson
import           Servant
import           Submit.Models
import           Submit.Config
import           Submit.API.Teachers
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Database.Persist.Postgresql (Entity, selectList)
import           Database.Esqueleto as E
import           Data.Text
import           Data.Maybe
import           Data.Time
import           GHC.Generics (Generic)

data CourseInfo = CourseInfo
        { courseInfoCourseCode  :: Text
        , courseInfoDescription :: Text
        , courseInfoName        :: Text
        , courseInfoTeachers    :: [TeacherInfo]
        -- , courseInfoAssistants  :: [StudentInfo]
        , assignments           :: [Entity Assignment]
        } deriving (Generic, Show)

instance ToJSON   CourseInfo
instance FromJSON CourseInfo

toCourseInfo :: Entity Course -> [TeacherInfo] -> [Entity Assignment] -> CourseInfo
toCourseInfo (Entity cid c) ts as = CourseInfo (courseCoursecode c) (courseDescription c) (courseCoursename c) ts as

-- host/courses/AFP returns data of course with coursecode "AFP"
type DetailedCourseAPI = "courses" :> Capture "coursecode" Text :> Get '[JSON] (Maybe CourseInfo)
-- host/courses returns general data about all courses
type AllCoursesAPI     = "courses" :> Get '[JSON] [Entity Course]
-- host/mycourses/:key: returns detailed data about the courses a student is following
type MyCoursesAPI      = "mycourses" :> Get '[JSON] [CourseInfo]
-- host/myteachings/:key: returns detailed data about the courses a teacher is teaching
type MyTeachingsAPI    = "myteachings" :> Get '[JSON] [CourseInfo]
-- All courses related API endpoints
type CoursesAPI        = DetailedCourseAPI :<|> AllCoursesAPI :<|> MyCoursesAPI :<|> MyTeachingsAPI
-- Server for CoursesAPI
coursesServer :: Config -> UserAuth -> Server CoursesAPI
coursesServer cfg ua = detailedCourseServer cfg :<|> allCoursesServer cfg :<|> (myCoursesServer cfg ua) :<|> (myTeachingsServer cfg ua)



detailedCourseServerT :: MonadIO m => ServerT DetailedCourseAPI (AppT m)
detailedCourseServerT = runDb . getCourse

detailedCourseProxy :: Proxy DetailedCourseAPI
detailedCourseProxy = Proxy

detailedCourseServer :: Config -> Server DetailedCourseAPI
detailedCourseServer cfg t = Handler $ runReaderT 
        (hoistServer detailedCourseProxy runApp detailedCourseServerT $ t) 
        cfg


allCoursesServerT :: MonadIO m => ServerT AllCoursesAPI (AppT m)
allCoursesServerT = runDb getAllCourses

allCoursesProxy :: Proxy AllCoursesAPI
allCoursesProxy = Proxy

allCoursesServer :: Config -> Server AllCoursesAPI
allCoursesServer cfg = Handler $ runReaderT
        (hoistServer allCoursesProxy runApp allCoursesServerT)
        cfg


myCoursesServerT :: MonadIO m => Key Student -> ServerT MyCoursesAPI (AppT m)
myCoursesServerT = runDb . getMyCourses

myCoursesProxy :: Proxy MyCoursesAPI
myCoursesProxy = Proxy

myCoursesServer :: Config -> UserAuth -> Server MyCoursesAPI
myCoursesServer cfg ua = case studentid ua of
    Nothing -> return []
    (Just k) -> Handler $ runReaderT
        (hoistServer myCoursesProxy runApp $ myCoursesServerT k)
        cfg


myTeachingsServerT :: MonadIO m => Key Teacher -> ServerT MyTeachingsAPI (AppT m)
myTeachingsServerT = runDb . getMyTeachings

myTeachingsProxy :: Proxy MyTeachingsAPI
myTeachingsProxy = Proxy

myTeachingsServer :: Config -> UserAuth -> Server MyTeachingsAPI
myTeachingsServer cfg ua = case teacherid ua of
    Nothing -> return []
    (Just k) -> Handler $ runReaderT
        (hoistServer myTeachingsProxy runApp $ myTeachingsServerT k)
        cfg


getCourseAssignments :: MonadIO m => (Entity Course) -> SqlPersistT m [Entity Assignment]
getCourseAssignments (Entity cid _) = do
    t <- liftIO getCurrentTime
    s <- E.select $
              from $ \a -> do
              where_ ((a ^. AssignmentCoursecode E.==. val cid) E.&&. (val t E.<. a ^. AssignmentDeadline))
              return a
    return s

getCourseTeachers :: MonadIO m => (Entity Course) -> SqlPersistT m [TeacherInfo]
getCourseTeachers (Entity cid _) = do
    s <- E.select $
              from $ \(tc,u,t) -> do
              where_ ((tc ^. TeachesCourseid E.==. val cid) E.&&. (tc ^. TeachesTeacherid E.==. t ^. TeacherId) E.&&. (t ^. TeacherUserid E.==. u ^. UserId))
              return (u, t)
    return $ fmap toTeacherInfo s

courseToCourseInfoQuery :: MonadIO m => Entity Course -> SqlPersistT m CourseInfo
courseToCourseInfoQuery c = do
    t <- getCourseTeachers c
    as <- getCourseAssignments c
    return $ toCourseInfo c t as

getCourse :: MonadIO m => Text -> SqlPersistT m (Maybe CourseInfo)
getCourse code = do
    s <- E.select $
              from $ \c -> do
              where_ (c ^. CourseCoursecode E.==. val code)
              return c
    let c = listToMaybe s
    maybe (return Nothing) (\x -> Just <$> (courseToCourseInfoQuery x)) c


getAllCourses :: MonadIO m => SqlPersistT m [Entity Course]
getAllCourses = E.select $
                    from $ \c -> do
                    where_ (c ^. CourseCoursecode E.==. c ^. CourseCoursecode) -- This seems redundant, but it tells Esqueleto that we need Entity Course values
                    return c

getMyCourses :: MonadIO m => Key Student -> SqlPersistT m [CourseInfo]
getMyCourses u = do
    cs <- E.select $
              from $ \(f,c) -> do
              where_ ((f ^. FollowsStudentid E.==. val u) E.&&. (f ^. FollowsCourseid E.==. c ^. CourseId))
              return c
    let cis = fmap courseToCourseInfoQuery cs
    sequenceA cis

getMyTeachings :: MonadIO m => Key Teacher -> SqlPersistT m [CourseInfo]
getMyTeachings u = do
    cs <- E.select $
              from $ \(f,c) -> do
              where_ ((f ^. TeachesTeacherid E.==. val u) E.&&. (f ^. TeachesCourseid E.==. c ^. CourseId))
              return c
    let cis = fmap courseToCourseInfoQuery cs
    sequenceA cis