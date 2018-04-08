{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Submit.API.Submissions where

import           Data.Aeson
import           Servant
import           Servant.Multipart
import           System.IO
import           System.Directory
import           System.FilePath
import           Submit.Models
import           Submit.Config
import           Submit.API.Courses
import           Submit.API.Teachers
import           Submit.API.Assignment
import           Submit.API.Students
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy.Char8 as BC
import           Database.Persist.Postgresql (Entity, selectList)
import           Database.Esqueleto as E
import           Data.Text
import           Data.Maybe
import           Data.Time
import           GHC.Generics (Generic)

data SubmissionInfo = SubmissionInfo
        { submissionInfoAssignment      :: Entity Assignment
        , submissionInfoStudents        :: [StudentInfo]
        , submissionInfoFiles           :: [Entity File]
        , submissionInfoLastchangedtime :: UTCTime
        , submissionInfoReadme          :: Text
        , submissionInfoGrade           :: Maybe (Double)
        , submissionInfoGradedby        :: Maybe (Entity User)
        , submissionInfoId              :: Key Submission
        } deriving (Show, Generic)

instance ToJSON SubmissionInfo
instance FromJSON SubmissionInfo

type SubmissionsAPI = MySubmissionsAPI :<|> UploadFileAPI :<|> AllSubmissionsAPI :<|> TeacherSubmissionAPI :<|> GradeAPI

submissionsServer :: Config -> UserAuth -> Server SubmissionsAPI
submissionsServer cfg ua = mySubmissionServer cfg ua 
                      :<|> uploadFileServer cfg ua 
                      :<|> allSubmissionsServer cfg ua
                      :<|> teacherSubmissionServer cfg ua
                      :<|> gradeServer cfg ua

type UploadFileAPI = "upload" :> Capture "submissionId" (Key Submission) :> MultipartForm Tmp FileUpload :> Post '[JSON] (Entity File)


data FileUpload = FileUpload
    { fileUploadPath :: FilePath
    , fileName       :: Text
    } deriving (Show, Generic)

instance FromMultipart Tmp FileUpload where
    fromMultipart multipartData = do
        let filedata = lookupFile "file" multipartData
        path <- fdPayload <$> filedata
        name <- fdFileName <$> filedata
        return $ FileUpload path name

type TempAPI = "upload" :> Capture "submissionId" (Key Submission) :> MultipartForm Tmp FileUpload :> Post '[JSON] (Maybe (Entity File))

uploadFileServer :: Config -> UserAuth -> Server UploadFileAPI
uploadFileServer cfg ua k f = (Handler $ (runReaderT $ (hoistServer (Proxy :: Proxy TempAPI) runApp (uploadFileServerT ua)) k f) cfg) >>=
    (Handler . ExceptT . return . (maybe (Left err401) Right))

uploadFileServerT :: MonadIO m => UserAuth -> ServerT TempAPI (AppT m)
uploadFileServerT ua k f = runDb $ processFile ua k f 

uploadFileProxy :: Proxy UploadFileAPI
uploadFileProxy = Proxy

type AllSubmissionsAPI = "allsubmissions" :> Capture "assignmentId" (Key Assignment) :> Get '[JSON] [SubmissionInfo]

allSubmissionsServer :: Config -> UserAuth -> Server AllSubmissionsAPI
allSubmissionsServer cfg ua k = Handler $ (runReaderT $ (hoistServer allSubmissionsProxy runApp (allSubmissionsServerT ua)) k) cfg

allSubmissionsServerT :: MonadIO m => UserAuth -> ServerT AllSubmissionsAPI (AppT m)
allSubmissionsServerT ua = runDb . getAllSubmissions ua

allSubmissionsProxy :: Proxy AllSubmissionsAPI
allSubmissionsProxy = Proxy

type MySubmissionsAPI = "mysubmission" :> Capture "assignmentId" (Key Assignment) :> Get '[JSON] (Maybe SubmissionInfo)

toSubmissionInfo :: Entity Submission -> Entity Assignment -> [StudentInfo] -> [Entity File] -> Maybe (Entity User) -> SubmissionInfo
toSubmissionInfo (Entity sid s) a us fs g = SubmissionInfo a us fs (submissionLastchangedtime s) (submissionReadme s) (submissionGrade s) g sid

mySubmissionServer :: Config -> UserAuth -> Server MySubmissionsAPI
mySubmissionServer cfg ua k = Handler $ (runReaderT $ (hoistServer mySubmissionProxy runApp (mySubmissionServerT ua)) k) cfg

mySubmissionServerT :: MonadIO m => UserAuth -> ServerT MySubmissionsAPI (AppT m)
mySubmissionServerT ua = runDb . getMySubmission ua

mySubmissionProxy :: Proxy MySubmissionsAPI
mySubmissionProxy = Proxy

type TeacherSubmissionAPI = "submissions" :> Capture "submissionId" (Key Submission) :> Get '[JSON] (Maybe SubmissionInfo)

teacherSubmissionServer :: Config -> UserAuth -> Server TeacherSubmissionAPI
teacherSubmissionServer cfg ua k = Handler $ (runReaderT $ (hoistServer teacherSubmissionProxy runApp (teacherSubmissionServerT ua)) k) cfg

teacherSubmissionServerT :: MonadIO m => UserAuth -> ServerT TeacherSubmissionAPI (AppT m)
teacherSubmissionServerT ua = runDb . getTeacherSubmission ua

teacherSubmissionProxy :: Proxy TeacherSubmissionAPI
teacherSubmissionProxy = Proxy

type GradeAPI = "grade" :> Capture "submissionId" (Key Submission) :> Capture "grade" Double :> Get '[JSON] Bool

gradeServer :: Config -> UserAuth -> Server GradeAPI
gradeServer cfg ua k d = Handler $ (runReaderT $ (hoistServer gradeProxy runApp (gradeServerT ua)) k d) cfg

gradeServerT :: MonadIO m => UserAuth -> ServerT GradeAPI (AppT m)
gradeServerT ua k = runDb . (updateGrade ua k)

gradeProxy :: Proxy GradeAPI
gradeProxy = Proxy

updateGrade :: MonadIO m => UserAuth -> Key Submission -> Double -> SqlPersistT m Bool
updateGrade ua k d = case teacherid ua of
    Nothing -> return False
    (Just tid) -> do
        bs <- E.select $
            from $ \(a,c,t,s) -> do
            where_ ((s ^. SubmissionId E.==. val k) E.&&.
                    (s ^. SubmissionAssignmentid E.==. a ^. AssignmentId) E.&&.
                    (a ^. AssignmentCoursecode E.==. c ^. CourseId) E.&&.
                    (t ^. TeachesCourseid E.==. c ^. CourseId) E.&&.
                    (t ^. TeachesTeacherid E.==. val tid))
        let b = listToMaybe bs
        case b of
            Nothing -> return False
            (Just _) -> do
                E.update $ \s -> do
                    E.set s [SubmissionGrade E.=. val (Just d), SubmissionGradedby E.=. val (Just $ userid ua)]
                    where_ (s ^. SubmissionId E.==. val k)
                return True


-- | Retrieve information about a specific submission. Only accesable by the submission's assignment's
--   course's teachers
getTeacherSubmission :: MonadIO m => UserAuth -> Key Submission -> SqlPersistT m (Maybe SubmissionInfo)
getTeacherSubmission ua k = case teacherid ua of
    Nothing -> return Nothing
    (Just tid) -> do
        ss <- E.select $
            from $ \(a,c,t,s) -> do
            where_ ((s ^. SubmissionAssignmentid E.==. a ^. AssignmentId) E.&&.
                    (a ^. AssignmentCoursecode E.==. c ^. CourseId) E.&&.
                    (t ^. TeachesCourseid E.==. c ^. CourseId) E.&&.
                    (t ^. TeachesTeacherid E.==. val tid))
            return (a,s)
        let s = listToMaybe ss
        case s of
            Nothing -> return Nothing
            (Just (a,s)) -> Just <$> toSubmissionInfoQuery a s

-- | Retrieve all submissions for a teacher's assignment.
getAllSubmissions :: MonadIO m => UserAuth -> Key Assignment -> SqlPersistT m [SubmissionInfo]
getAllSubmissions ua k = case teacherid ua of
    Nothing -> return []
    (Just tid) -> do
        ss <- E.select $
            from $ \(a,c,t) -> do
            where_ ((a ^. AssignmentId E.==. val k) E.&&. (a ^. AssignmentCoursecode E.==. c ^. CourseId) 
                    E.&&. (c ^. CourseId E.==. t ^. TeachesCourseid) E.&&. (t ^. TeachesTeacherid E.==. val tid))
            return a
        let s = listToMaybe ss
        case s of
            Nothing -> return []
            (Just a) -> do
                subs <- E.select $
                    from $ \su -> do
                    where_ (su ^. SubmissionAssignmentid E.==. val (entityKey a))
                    return su
                sequenceA $ fmap (toSubmissionInfoQuery a) subs
                

-- | Retrieve a student's submission for a certain assignment.
getMySubmission :: MonadIO m => UserAuth -> Key Assignment -> SqlPersistT m (Maybe SubmissionInfo)
getMySubmission ua k = case studentid ua of
    Nothing -> return Nothing
    (Just sid) -> do
        ss <- E.select $
            from $ \(a,c,f) -> do
            where_ ((a ^. AssignmentId E.==. val k) E.&&. (a ^. AssignmentCoursecode E.==. c ^. CourseId) 
                    E.&&. (c ^. CourseId E.==. f ^. FollowsCourseid) E.&&. (f ^. FollowsStudentid E.==. val sid))
            return a
        let s = listToMaybe ss
        case s of
            Nothing -> return Nothing
            (Just a) -> do
                subs <- E.select $
                    from $ \su -> do
                    where_ (su ^. SubmissionAssignmentid E.==. val (entityKey a))
                    return su
                let sub = listToMaybe subs
                case sub of
                    Nothing -> return Nothing
                    (Just subm) -> Just <$> toSubmissionInfoQuery a subm

toSubmissionInfoQuery :: (MonadIO m, BackendCompatible SqlBackend backend, PersistQueryRead backend, PersistUniqueRead backend) =>
        Entity Assignment -> Entity Submission -> ReaderT backend m SubmissionInfo
toSubmissionInfoQuery a subm = do
    students <- E.select $
        from $ \(stu,stusub,u) -> do
        where_ ((stu ^. StudentId E.==. stusub ^. SubmitsStudentid) E.&&. (stusub ^. SubmitsSubmissionid E.==. val (entityKey subm)) E.&&.
            (stu ^. StudentUserid E.==. u ^. UserId))
        return (u, stu)
    files <- E.select $
        from $ \fs -> do
        where_ (fs ^. FileSubmissionid E.==. val (entityKey subm))
        return fs
        
    grader <- case submissionGradedby $ entityVal subm of
        Nothing -> return []
        (Just x) -> E.select $
            from $ \users -> do
            where_ (users ^. UserId E.==. val x)
            return users
    return $ toSubmissionInfo subm a (fmap toStudentInfo students) files (listToMaybe grader)

processFile :: MonadIO m => UserAuth -> Key Submission -> FileUpload -> SqlPersistT m (Maybe (Entity File))
processFile ua ks (FileUpload path name) = case studentid ua of 
    Nothing    -> return Nothing
    (Just sid) -> do
        time <- liftIO getCurrentTime
        ss <- E.select $
            from $ \(sits, ssion, ass) -> do
            where_ ((sits ^. SubmitsSubmissionid E.==. ssion ^. SubmissionId) E.&&. 
                    (sits ^. SubmitsStudentid E.==. val sid) E.&&.
                    (ssion ^. SubmissionAssignmentid E.==. ass ^. AssignmentId) E.&&.
                    (ass ^. AssignmentDeadline E.>. val time) E.&&.
                    (ssion ^. SubmissionId E.==. val ks))
            return (ssion, ass)
        let s = listToMaybe ss
        case s of
            Nothing -> return Nothing
            (Just (submission, assignment)) -> do
                f <- liftIO moveAndGetFile
                fk <- insert f
                return $ Just $ Entity fk f
    where moveAndGetFile :: IO File
          moveAndGetFile = do
              let newpath = "assets/files/" ++ (takeFileName path)
              c <- copyFile path newpath
              h <- openFile newpath ReadMode
              size <- hFileSize h
              return $ File False (fromIntegral size) name (pack newpath) ks