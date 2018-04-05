{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}

module Submit.API.Assignment 
    ( AssignmentAPI
    , assignmentServer
    ) where

import           Data.Aeson
import           Servant
import           Submit.Models
import           Submit.Config
import           Submit.API.Courses
import           Submit.API.Teachers
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Database.Persist.Postgresql (Entity, selectList)
import           Database.Esqueleto as E
import           Data.Text
import           Data.Maybe
import           Data.Time
import           GHC.Generics (Generic)

data AssignmentInfo = AssignmentInfo
        { assignmentInfoCourse       :: Entity Course
        , assignmentInfoName         :: Text
        , assignmentInfoDescription  :: Text
        , assignmentInfoDeadline     :: UTCTime
        , assignmentInfoNrofstudents :: Int
        , assignmentInfoFollowing    :: Bool
        , assignmentInfoTeaching     :: Bool
        } deriving (Generic, Show)

instance ToJSON   AssignmentInfo
instance FromJSON AssignmentInfo

toAssignmentInfo :: Entity Assignment -> Entity Course -> Bool -> Bool -> AssignmentInfo
toAssignmentInfo (Entity aid a) c f t = AssignmentInfo c (assignmentName a) (assignmentDescription a) 
                                        (assignmentDeadline a) (assignmentNrofstudents a) f t

type AssignmentAPI = "assignments" :> Capture "assignmentId" (Key Assignment) :> Get '[JSON] (Maybe AssignmentInfo)

assignmentServer :: Config -> UserAuth -> Server AssignmentAPI
assignmentServer cfg ua k = Handler $ (runReaderT $ (hoistServer assignmentProxy runApp (assignmentServerT ua)) k) cfg

assignmentServerT :: MonadIO m => UserAuth -> ServerT AssignmentAPI (AppT m)
assignmentServerT ua = runDb . getAssignment ua 

assignmentProxy :: Proxy AssignmentAPI
assignmentProxy = Proxy

getAssignment :: MonadIO m => UserAuth -> Key Assignment -> SqlPersistT m (Maybe AssignmentInfo)
getAssignment ua k = do
    s <- E.select $
              from $ \(a,c) -> do
              where_ ((a ^. AssignmentId E.==. val k) E.&&. (a ^. AssignmentCoursecode E.==. c ^. CourseId))
              return (a,c)
    let sh = listToMaybe s

    case sh of
        Nothing -> return Nothing
        (Just (a,c)) -> do
            follows <- case studentid ua of
                    Nothing -> return False
                    (Just sk) -> do
                        fs <- E.select $
                                  from $ \f -> do
                                  where_ ((f ^. FollowsStudentid E.==. val sk) E.&&. (f ^. FollowsCourseid E.==. val (entityKey c)))
                                  return f
                        return $ Prelude.length fs > 0
            teaches <- case teacherid ua of
                    Nothing -> return False
                    (Just tk) -> do
                        ts <- E.select $
                                  from $ \t -> do
                                  where_ ((t ^. TeachesTeacherid E.==. val tk) E.&&. (t ^. TeachesCourseid E.==. val (entityKey c)))
                                  return t
                        return $ Prelude.length ts > 0
                    
            return $ Just $ toAssignmentInfo a c follows teaches