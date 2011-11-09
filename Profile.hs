{-# LANGUAGE OverloadedStrings #-}

module Profile where

import Control.Monad.Trans
import Data.Bson
import Data.Time.Clock
import Database.MongoDB

data FSPost = FSPost {
  postAuthorId :: ObjectId,
  postTimestamp :: UTCTime,
  postText :: UString
} deriving (Show)

data FSProfile = FSProfile {
  profileId :: Maybe ObjectId,
  firstName :: UString,
  middleName :: UString,
  lastName :: UString,
  currentCity :: UString,
  friends :: [ObjectId],
  incomingFriendRequests :: [ObjectId],
  posts :: [FSPost]
} deriving (Show)

defaultFSProfile :: FSProfile
defaultFSProfile = FSProfile {
  profileId = Nothing, firstName = "", middleName = "", lastName = "",
  currentCity = "", friends = [], incomingFriendRequests = [], posts = []
}

atOrDefault :: Val v => Label -> Document -> v -> v
atOrDefault key doc def = maybe def id (Database.MongoDB.lookup key doc)

documentToProfile ::  Document -> FSProfile
documentToProfile doc = defaultFSProfile {
  profileId = at "_id" doc,
  firstName = at "first_name" doc,
  middleName = atOrDefault "middle_name" doc "",
  lastName = at "last_name" doc,
  currentCity = atOrDefault "current_city" doc "",
  friends = atOrDefault "friends" doc [],
  incomingFriendRequests = atOrDefault "incoming_friend_requests" doc [],
  posts = map documentToPost $ atOrDefault "posts" doc []
}

documentToPost :: Document -> FSPost
documentToPost doc = FSPost {
  postAuthorId = at "author_id" doc,
  postTimestamp = at "timestamp" doc,
  postText = at "text" doc
}

findProfile :: MonadIO m => ObjectId -> Action m FSProfile
findProfile id = do
  dbProfile <- fetch (select ["_id" =: id] "profiles")
  return $ documentToProfile dbProfile


run act = do
  pipe <- runIOE $ connect $ host "127.0.0.1"
  (Right profiles) <- access pipe master "friendstar" act
  return profiles
