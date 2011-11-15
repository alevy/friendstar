{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Profile where

import Prelude hiding (lookup)

import Control.Applicative
import Control.Monad.Trans
import Data.Bson
import Data.Bool
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Data
import Data.Map
import qualified Data.Text as T
import Data.Time.Clock
import Data.Typeable
import Data.Maybe
import Database.MongoDB
import Text.Regex

type FSObjectId = S.ByteString

fromObjectId :: ObjectId -> FSObjectId
fromObjectId id = S.pack $ show id

toObjectId :: FSObjectId -> ObjectId
toObjectId id = read $ S.unpack id

data FSPost = FSPost {
  postAuthorId :: FSObjectId,
  postTimestamp :: UTCTime,
  postText :: T.Text
} deriving (Show, Eq, Data, Typeable)

data FSProfile = FSProfile {
  profileId :: Maybe FSObjectId,
  username :: String,
  firstName :: String,
  middleName :: Maybe String,
  lastName :: String,
  currentCity :: Maybe String,
  friends :: [FSObjectId],
  incomingFriendRequests :: [FSObjectId],
  posts :: [FSPost]
} deriving (Show, Eq, Data, Typeable)

defaultFSProfile :: FSProfile
defaultFSProfile = FSProfile {
  profileId = Nothing, username = "", firstName = "", middleName = Nothing,
  lastName = "", currentCity = Nothing, friends = [],
  incomingFriendRequests = [], posts = []
}

profileFromMap :: Map String L.ByteString -> FSProfile
profileFromMap map = defaultFSProfile {
  username = L.unpack $ map Data.Map.! "username",
  firstName = L.unpack $ map Data.Map.! "first_name",
  middleName = fmap L.unpack $ "middle_name" `Data.Map.lookup` map,
  lastName = L.unpack $ map Data.Map.! "last_name",
  currentCity = fmap L.unpack $ "current_city" `Data.Map.lookup` map
}

instance Val FSProfile where
  val profile = Doc [ "_id" =: (fmap toObjectId $ profileId profile),
		  "username" =: username profile,
                  "first_name" =: firstName profile,
                  "middle_name" =: middleName profile,
                  "last_name" =: lastName profile,
                  "current_city" =: currentCity profile,
                  "friends" =: (fmap toObjectId $ friends profile),
                  "incoming_friend_requests" =: (fmap toObjectId $ incomingFriendRequests profile),
                  "posts" =: posts profile]

  cast' (Doc doc) = Just defaultFSProfile {
    profileId = fmap fromObjectId $ at "_id" doc,
    username = at "username" doc,
    firstName = at "first_name" doc,
    middleName = Data.Bson.lookup "middle_name" doc,
    lastName = at "last_name" doc,
    currentCity = Data.Bson.lookup "current_city" doc,
    friends = fmap fromObjectId $ atOrDefault "friends" doc [],
    incomingFriendRequests = fmap fromObjectId $ atOrDefault "incoming_friend_requests" doc [],
    posts = fmap toPost $ atOrDefault "posts" doc []}
    where toPost post = fromJust $ cast' post


atOrDefault :: Val v => Label -> Document -> v -> v
atOrDefault key doc def = maybe def id (Database.MongoDB.lookup key doc)

instance Val FSPost where
  val post = Doc [ "author_id" =: (toObjectId $ postAuthorId post),
               "timestamp" =: postTimestamp post,
               "text" =: (T.unpack $ postText post)]

  cast' (Doc doc) = Just FSPost {
    postAuthorId = fromObjectId $ at "author_id" doc,
    postTimestamp = at "timestamp" doc,
    postText = T.pack $ at "text" doc}

{-
 - Profile Operations
 -}

findProfile :: MonadIO m => FSObjectId -> Action m FSProfile
findProfile id = do
  let objId = toObjectId id
  findProfileBy "_id" objId

findProfileByUsername :: MonadIO m => S.ByteString -> Action m FSProfile
findProfileByUsername username = do
  let strUsername = S.unpack username
  findProfileBy "username" strUsername

findProfileBy :: (MonadIO m, Val v) => Label -> v -> Action m FSProfile
findProfileBy key value = do
  dbProfile <- fetch (select [key =: value] "profiles")
  let (Just profile) = cast' (Doc dbProfile)
  return profile

saveProfile :: (MonadIO m, Applicative m) => FSProfile -> Action m FSProfile
saveProfile profile
  | isJust (profileId profile) = do
      save "profiles" $ doc
      return profile
  | otherwise = do
      save "profiles" $ exclude ["_id"] doc
      return profile
  where (Doc doc) = val profile

{-
 - Posts Operations
 -}

-- Post a FSPost to a profile
postToProfile :: (MonadIO m, Applicative m) => FSPost -> FSObjectId -> Action m FSPost
postToProfile post profileId = do
  modify (select ["_id" =: objId] "profiles") ["$push" =: ["posts" =: post_doc]]
  return post
  where (Doc post_doc) = val post
        objId = toObjectId profileId

{-
 - Friend List Manipulation
 -}

-- Add friend request to the specified user profile
requestFriendship :: (MonadIO m, Applicative m) => FSObjectId -> FSObjectId -> Action m FSObjectId
requestFriendship fromUser toUser = do
  modify (select ["_id" =: toId] "profiles") ["$push" =: ["incoming_friend_requests" =: fromId]]
  return fromUser
  where fromId = toObjectId fromUser
        toId = toObjectId toUser

-- Test if a friend request already exists
friendshipRequestExists :: (MonadIO m) => FSProfile -> FSProfile -> m Bool
friendshipRequestExists myProfile friendProfile
  | isJust myProfileId = do
    return $ elem (fromJust myProfileId) friendRequests
  | otherwise = do
    return False
    -- Need force this to error out in a rational way but this is a safe bet.
  where friendRequests = incomingFriendRequests friendProfile
        myProfileId = profileId myProfile

-- Retrieve the friend requests

-- Accept a friend request

run act = do
  pipe <- runIOE $ connect $ host "127.0.0.1"
  (Right profiles) <- access pipe master "friendstar" act
  return profiles

