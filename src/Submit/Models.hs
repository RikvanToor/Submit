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
import           Submit.Config


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    username Text
    UniqueUsername username
    name Text
    password Text
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
    coursecode CourseId
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
    lastchangedtime UTCTime
    readme Text
    grade Double
    gradedby UserId
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

connStr2 = "host=localhost dbname=submit user=submit password=test port=5432"

buildDb :: SqlPersistT IO ()
buildDb = runMigration migrateAll 

-- runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks configPool
    liftIO $ runSqlPool query pool