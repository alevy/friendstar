{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Profile where

import Control.Monad.Trans
import Data.Bson
import Data.Time.Clock
import Data.Typeable
import Database.MongoDB

data FSPost = FSPost {
  postAuthorId :: ObjectId,
  postTimestamp :: UTCTime,
  postText :: UString
} deriving (Show, Eq, Typeable)

data FSProfile = FSProfile {
  profileId :: Maybe ObjectId,
  firstName :: UString,
  middleName :: UString,
  lastName :: UString,
  currentCity :: UString,
  friends :: [ObjectId],
  incomingFriendRequests :: [ObjectId],
  posts :: [FSPost]
} deriving (Show, Eq, Typeable)

defaultFSProfile :: FSProfile
defaultFSProfile = FSProfile {
  profileId = Nothing, firstName = "", middleName = "", lastName = "",
  currentCity = "", friends = [], incomingFriendRequests = [], posts = []
}

instance Val FSProfile where
  val profile = Doc [ "_id" =: profileId profile,
                  "first_name" =: firstName profile,
                  "middle_name" =: middleName profile,
                  "last_name" =: lastName profile,
                  "current_city" =: currentCity profile]
                 -- "friends" =: friends profile,
                 -- "incoming_friend_requests" := incomingFriendRequests profile,
                 -- "posts" := posts profile] :: Document

  cast' doc = Just defaultFSProfile {
    profileId = at "_id" doc,
    firstName = at "first_name" doc,
    middleName = atOrDefault "middle_name" doc "",
    lastName = at "last_name" doc,
    currentCity = atOrDefault "current_city" doc "",
    friends = atOrDefault "friends" doc [],
    incomingFriendRequests = atOrDefault "incoming_friend_requests" doc [],
    posts = val $ atOrDefault "posts" doc []}


atOrDefault :: Val v => Label -> Document -> v -> v
atOrDefault key doc def = maybe def id (Database.MongoDB.lookup key doc)

instance Val FSPost where
  val post = [ "author_id" =: postAuthorId post,
               "timestamp" =: postTimestamp post,
               "text" =: postText post]

  cast' doc = FSPost {
    postAuthorId = at "author_id" doc,
    postTimestamp = at "timestamp" doc,
    postText = at "text" doc}

findProfile :: MonadIO m => ObjectId -> Action m FSProfile
findProfile id = do
  dbProfile <- fetch (select ["_id" =: id] "profiles")
  return $ val dbProfile

saveProfile :: MonadIO m => FSProfile -> Action m FSProfile
saveProfile profile = insert "profiles" $ cast' profile


run act = do
  pipe <- runIOE $ connect $ host "127.0.0.1"
  (Right profiles) <- access pipe master "friendstar" act
  return profiles

