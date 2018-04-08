{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}

module Submit.API.Students where

import           Data.Aeson
import           Servant
import           Submit.Models
import           Submit.Config
import           Control.Monad.Reader
import           Database.Persist.Postgresql (Entity, selectList)
import           Database.Esqueleto as E
import           Data.Text
import           GHC.Generics (Generic)

-- | Represents a student containing the following info: userID, name and year of enrollment
data StudentInfo = StudentInfo
        { studentInfoUserId         :: UserId
        , studentInfoName           :: Text
        , studentInfoEnrollmentyear :: Int
        } deriving (Generic, Eq, Show)

instance FromJSON StudentInfo
instance ToJSON   StudentInfo

toStudentInfo :: (Entity User, Entity Student) -> StudentInfo
toStudentInfo (Entity uid u, Entity sid s) = StudentInfo uid (userName u) (studentEnrollmentyear s)