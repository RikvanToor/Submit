{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}

module Submit.Models where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO, ReaderT)
import           Control.Monad.Logger (runStderrLoggingT)
import           Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import           Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)
import           Database.Persist.Postgresql
import qualified Database.Esqueleto as E
import           Database.Esqueleto   ((^.))
import           Data.Text
import           Data.Time.Clock (UTCTime)
import           Data.Aeson
import           GHC.Generics
import           Submit.Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    username Text
    UniqueUsername username
    password Text
    name Text
    deriving Show Eq 
Student json
    userid UserId
    UniqueStudentid userid
    enrollmentyear Int
    deriving Show Eq
Teacher json
    userid UserId
    UniqueTeacherid userid
    office Text
    deriving Show Eq
Course json
    coursecode Text
    description Text
    coursename Text
    UniqueCourseCode coursecode
    deriving Show Eq
Assignment json
    coursecode  CourseId
    name Text
    description Text
    deadline UTCTime
    nrofstudents Int
    deriving Show Eq
Submission json
    assignmentid AssignmentId
    lastchangedtime UTCTime
    readme Text
    grade Double Maybe
    gradedby UserId Maybe
    deriving Show Eq
File json
    isbinary Bool
    filesize Int
    filename Text
    submissionid SubmissionId
    deriving Show Eq
Follows json
    studentid StudentId
    courseid CourseId
    UniqueFollows studentid courseid
    deriving Show Eq
Teaches json
    teacherid TeacherId
    courseid CourseId
    UniqueTeaches teacherid courseid
    deriving Show Eq
Assists json
    studentid StudentId
    courseid CourseId
    UniqueAssists studentid courseid
    deriving Show Eq
Submits json
    studentid StudentId
    submissionid SubmissionId
    UniqueSubmits studentid submissionid
    deriving Show Eq
|]

buildDb :: SqlPersistT IO ()
buildDb = runMigration migrateAll 

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks configPool
    liftIO $ runSqlPool query pool


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