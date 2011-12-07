{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Profile where

import Prelude hiding (lookup)

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.IO.Control
import Data.Bson
import Data.Bool
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Data
import qualified Data.List as List
import Data.Map hiding (map)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Typeable
import Data.Maybe
import Data.String.Utils
import Data.UString (u)
import Database.MongoDB
import Text.PhoneticCode.Soundex
import Text.Regex
import System.IO.Unsafe

import qualified LIO.TCB as LIO
import qualified LIO.DCLabel as DC

import FSDB

data FSPost = FSPost {
  postAuthorId :: FSObjectId,
  postTimestamp :: UTCTime,
  postText :: T.Text
} deriving (Show, Eq, Data, Typeable)

instance Ord FSPost where
  compare post1 post2 = compare (postTimestamp post2) (postTimestamp post1)

data FSProfile = FSProfile {
  profileId :: Maybe FSObjectId,
  profilePicId :: Maybe FSObjectId,
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
  profileId = Nothing, profilePicId = Nothing, username = "", firstName = "", middleName = Nothing,
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

searchTerms :: FSProfile -> [String]
searchTerms profile = map soundexNARA terms
  where terms = [firstName profile,
                 maybe "" id $ middleName profile,
                 lastName profile,
                 username profile] ++
                 (splitWs $ maybe "" id $ currentCity profile)

instance Val FSProfile where
  val profile = Doc [ "_id" =: (fmap toObjectId $ profileId profile),
                  "profile_pic_id" =: (fmap toObjectId $ profilePicId profile),
                  "username" =: username profile,
                  "first_name" =: firstName profile,
                  "middle_name" =: middleName profile,
                  "last_name" =: lastName profile,
                  "current_city" =: currentCity profile,
                  "search_terms" =: searchTerms profile,
                  "friends" =: (fmap toObjectId $ friends profile),
                  "incoming_friend_requests" =: (fmap toObjectId $ incomingFriendRequests profile),
                  "posts" =: posts profile]

  cast' (Doc doc) = Just defaultFSProfile {
    profileId = fmap fromObjectId $ at "_id" doc,
    profilePicId = fmap fromObjectId $ Data.Bson.lookup "profile_pic_id" doc,
    username = at "username" doc,
    firstName = at "first_name" doc,
    middleName = Data.Bson.lookup "middle_name" doc,
    lastName = at "last_name" doc,
    currentCity = Data.Bson.lookup "current_city" doc,
    friends = fmap fromObjectId $ atOrDefault "friends" doc [],
    incomingFriendRequests = fmap fromObjectId $ atOrDefault "incoming_friend_requests" doc [],
    posts = List.sort $ fmap toPost $ atOrDefault "posts" doc []}
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

lfindProfile :: MonadIO m => FSObjectId -> DC.DC (Action m FSProfile)
lfindProfile id = do
  let objId = toObjectId id
  LIO.evaluate $ findProfileBy "_id" objId

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

searchProfiles :: (MonadControlIO m, MonadIO m, Applicative m) => String -> Int -> Action m [FSProfile]
searchProfiles query maxResults = do
  let normalizedQuery = map soundexNARA $ splitWs query
  let selector = map ("search_terms" =:) normalizedQuery
  liftIO $ putStrLn $ show selector
  cursor <- find $ select selector "profiles"
  values <- nextN maxResults cursor
  return $ map (fromJust . cast' . Doc) values

saveProfile :: (MonadIO m, Applicative m) => FSProfile -> Action m FSProfile
saveProfile profile
  | isJust (profileId profile) = do
      save "profiles" $ doc
      return profile
  | otherwise = do
      save "profiles" $ exclude ["_id"] doc
      return profile
  where (Doc doc) = val profile

-- For debugging list all profiles
{-
listProfiles :: MonadIO m => () -> Action m [FSProfile]
listProfiles _ = do
  profiles <- find (select [] "profiles")
  return $ fmap (cast' . Doc) (rest profiles)
-}

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

data FSPostWithAuthor = FSPostWithAuthor {
  post :: FSPost, author :: FSProfile
  } deriving (Show, Data, Typeable)

postWithAuthor :: (MonadIO m, Applicative m) => FSPost -> Action m FSPostWithAuthor
postWithAuthor post = do
  author <- findProfile $ postAuthorId post
  return $ FSPostWithAuthor post author

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
friendshipRequestExists :: FSProfile -> FSProfile -> Bool
friendshipRequestExists myProfile friendProfile
  | isJust friendProfileId = do
    elem (fromJust friendProfileId) friendRequests
  | otherwise = do
    False
    -- Need force this to error out in a rational way but this is a safe bet.
  where friendRequests = incomingFriendRequests myProfile
        friendProfileId = profileId friendProfile

-- Accept a friend request
acceptFriendship :: (MonadIO m) => FSObjectId -> FSObjectId -> Action m FSObjectId
acceptFriendship myObjId friendObjId = do
  modify (select ["_id" =: myId] "profiles") ["$push" =: ["friends" =: friendId]]
  modify (select ["_id" =: friendId] "profiles") ["$push" =: ["friends" =: myId]]
  modify (select ["_id" =: myId] "profiles") ["$pull" =: ["incoming_friend_requests" =: friendId]]
  return friendObjId
  where myId = toObjectId myObjId
        friendId = toObjectId friendObjId

-- Remove friend or friend request
-- XXX: Probably we want to return an error on failure
removeFriendship :: (MonadIO m, Applicative m) => FSObjectId -> FSObjectId -> Action m FSObjectId
removeFriendship myObjId friendObjId = do
  modify (select ["_id" =: myId] "profiles") ["$pull" =: ["incoming_friend_requests" =: friendId]]
  modify (select ["_id" =: myId] "profiles") ["$pull" =: ["friends" =: friendId]]
  modify (select ["_id" =: friendId] "profiles") ["$pull" =: ["friends" =: myId]]
  return friendObjId
  where myId = toObjectId myObjId
        friendId = toObjectId friendObjId

server = runIOE $ connect $ host "127.0.0.1"

lrun :: DC.DC (Action IO a) -> DC.DC a
lrun lact = do
  let pipe = unsafePerformIO server
  do
    (act, state) <- DC.evalDC $ lact
    (Right result) <- access pipe master "friendstar" act
    return result


run act = unsafePerformIO $ do
  pipe <- server
  (Right result) <- access pipe master "friendstar" act
  return result

