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
import           Submit.Models
import           Submit.Config
import           Submit.API.Courses
import           Submit.API.Teachers
import           Submit.API.Assignment
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Database.Persist.Postgresql (Entity, selectList)
import           Database.Esqueleto as E
import           Data.Text
import           Data.Maybe
import           Data.Time
import           GHC.Generics (Generic)

data SubmissionInfo = SubmissionInfo
        { submissionInfoAssignment      :: Entity Assignment
        , submissionInfoStudents        :: [Entity Student]
        , submissionInfoFiles           :: [Entity File]
        , submissionInfoLastchangedtime :: UTCTime
        , submissionInfoReadme          :: Text
        , submissionInfoGrade           :: Maybe (Double)
        , submissionInfoGradedby       :: Maybe (Entity User)
        } deriving (Show, Generic)

instance ToJSON SubmissionInfo
instance FromJSON SubmissionInfo

type SubmissionsAPI = MySubmissionsAPI

submissionsServer :: Config -> UserAuth -> Server SubmissionsAPI
submissionsServer = mySubmissionServer

type UploadFileAPI = "upload" :> Capture "submissionId" (Key Submission) :> MultipartForm Tmp FileUpload :> Post '[JSON] (Entity File)

data FileUpload = FileUpload
    { fileUploadPath :: FilePath
    } deriving (Show, Generic)

instance FromMultipart Tmp FileUpload where
    fromMultipart multipartData = FileUpload <$> (fdPayload <$> lookupFile "file" multipartData)

uploadFileServer :: FileUpload -> Handler (Entity File)
uploadFileServer (FileUpload path) = undefined

type MySubmissionsAPI = "mysubmission" :> Capture "submissionId" (Key Assignment) :> Get '[JSON] (Maybe SubmissionInfo)

toSubmissionInfo :: Submission -> Entity Assignment -> [Entity Student] -> [Entity File] -> Maybe (Entity User) -> SubmissionInfo
toSubmissionInfo s a us fs g = SubmissionInfo a us fs (submissionLastchangedtime s) (submissionReadme s) (submissionGrade s) g

mySubmissionServer :: Config -> UserAuth -> Server MySubmissionsAPI
mySubmissionServer cfg ua k = Handler $ (runReaderT $ (hoistServer mySubmissionProxy runApp (mySubmissionServerT ua)) k) cfg

mySubmissionServerT :: MonadIO m => UserAuth -> ServerT MySubmissionsAPI (AppT m)
mySubmissionServerT ua = runDb . getMySubmission ua

mySubmissionProxy :: Proxy MySubmissionsAPI
mySubmissionProxy = Proxy


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
                    (Just subm) -> do
                        students <- E.select $
                            from $ \(stu,stusub) -> do
                            where_ ((stu ^. StudentId E.==. stusub ^. SubmitsStudentid) E.&&. (stusub ^. SubmitsSubmissionid E.==. val (entityKey subm)))
                            return stu
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
                        return $ Just $ toSubmissionInfo (entityVal subm) a students files (listToMaybe grader)



{-data User = User { username :: Text, pic :: FilePath }

instance FromMultipart Tmp User where
  fromMultipart multipartData =
    User <$> lookupInput "username" multipartData
         <*> fmap fdPayload (lookupFile "pic" multipartData)

type API = MultipartForm Tmp User :> Post '[PlainText] String

server :: User -> Handler String
server usr = return str

  where str = (unpack $ username usr) ++ "'s profile picture"
           ++ " got temporarily uploaded to "
           ++ pic usr ++ " and will be removed from there "
           ++ " after this handler has run."

proxyboi :: Proxy API
proxyboi = Proxy

test :: Application
test = serve proxyboi server
-}