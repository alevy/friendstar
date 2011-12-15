{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Profile where

import Prelude hiding (lookup)
import qualified Prelude (lookup)
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.IO.Control
import Data.Bson
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Data
import qualified Data.List as List
import Data.Map hiding (map, singleton)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Maybe
import qualified Data.UString as U
import Data.String.Utils
import Database.MongoDB
import Text.PhoneticCode.Soundex

import qualified LIO.TCB as LIO
import LIO.DCLabel hiding (label)

import FSDB hiding (run, runM)
import qualified FSDB as FSDB (run, runM)

runM = FSDB.runM
run = FSDB.run

data FSPost = FSPost {
  postAuthorId :: FSObjectId,
  postTimestamp :: UTCTime,
  postText :: T.Text
} deriving (Show, Eq, Typeable)

instance Ord FSPost where
  compare post1 post2 = compare (postTimestamp post2) (postTimestamp post1)

data FSProfile = FSProfile {
  profileId :: Maybe FSObjectId,
  profilePicId :: Maybe FSObjectId,
  username :: String,
  firstName :: String,
  middleName :: Maybe String,
  lastName :: String,
  currentCity :: LIO.Labeled DCLabel (Maybe String),
  friends :: [FSObjectId],
  incomingFriendRequests :: [FSObjectId],
  posts :: [FSPost],
  permissions :: [String]
} deriving (Typeable)

instance Eq FSProfile where
  p1 == p2 = (isJust $ profileId p1) && (profileId p1) == (profileId p2)

instance Show FSProfile where
  show p = "FSProfile [_id: \"" ++ (show $ profileId p) ++ "\", username: \"" ++ (username p) ++ "\"]"

defaultFSProfile :: FSProfile
defaultFSProfile = FSProfile {
  profileId = Nothing, profilePicId = Nothing, username = "", firstName = "", middleName = Nothing,
  lastName = "", currentCity = (LIO.labelTCB LIO.ltop Nothing), friends = [],
  incomingFriendRequests = [], posts = [], permissions = []
}

fullName :: FSProfile -> String
fullName p = firstName p ++ " " ++ lastName p

profileFromMap :: String -> Map String L.ByteString -> FSProfile
profileFromMap uname map = defaultFSProfile {
  username = uname,
  firstName = L.unpack $ map Data.Map.! "first_name",
  middleName = fmap L.unpack $ "middle_name" `Data.Map.lookup` map,
  lastName = L.unpack $ map Data.Map.! "last_name",
  currentCity = lbl $ fmap L.unpack $ "current_city" `Data.Map.lookup` map
}
  where lbl = LIO.labelTCB (newDC uname uname)

searchTerms :: FSProfile -> [String]
searchTerms profile = map soundexNARA terms
  where terms = [firstName profile,
                 maybe "" id $ middleName profile,
                 lastName profile,
                 username profile]

instance Val FSProfile where
  val profile = Doc [ "_id" =: (fmap toObjectId $ profileId profile),
                  "profile_pic_id" =: (fmap toObjectId $ profilePicId profile),
                  "username" =: username profile,
                  "first_name" =: firstName profile,
                  "middle_name" =: middleName profile,
                  "last_name" =: lastName profile,
                  "current_city" =: (LIO.unlabelTCB $ currentCity profile),
                  "search_terms" =: searchTerms profile,
                  "friends" =: (fmap toObjectId $ friends profile),
                  "incoming_friend_requests" =: (fmap toObjectId $ incomingFriendRequests profile),
                  "posts" =: posts profile,
                  "permissions" =: permissions profile]

  cast' (Doc doc) = Just defaultFSProfile {
    profileId = fmap fromObjectId $ at "_id" doc,
    profilePicId = fmap fromObjectId $ Data.Bson.lookup "profile_pic_id" doc,
    username = uname,
    firstName = at "first_name" doc,
    middleName = Data.Bson.lookup "middle_name" doc,
    lastName = at "last_name" doc,
    currentCity = lbl $ Data.Bson.lookup "current_city" doc,
    friends = fmap fromObjectId $ atOrDefault "friends" doc [],
    incomingFriendRequests = fmap fromObjectId $ atOrDefault "incoming_friend_requests" doc [],
    posts = List.sort $ fmap toPost $ atOrDefault "posts" doc [],
    permissions = perms}
    where toPost post = fromJust $ cast' post
          toPermission permission = fromJust $ cast' permission
          uname = at "username" doc
          lbl = LIO.labelTCB (newDC trustees uname)
          trustees = foldl (.\/.) (singleton uname) perms
          perms = atOrDefault "permissions" doc []
          orEmpty = maybe [] (fromJust)


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

findProfile :: MonadIO m => FSObjectId -> FSDBQuery m FSProfile
findProfile id = do
  let objId = toObjectId id
  findProfileBy "_id" objId

findProfileByUsername :: MonadIO m => S.ByteString -> FSDBQuery m FSProfile
findProfileByUsername username = do
  let strUsername = S.unpack username
  findProfileBy "username" strUsername

findProfileBy :: (MonadIO m, Val v) => Label -> v -> FSDBQuery m FSProfile
findProfileBy key value = FSDBQueryC $ do
  dbProfile <- fetch (select [key =: value] "profiles")
  let (Just profile) = cast' (Doc dbProfile)
  return profile

searchProfiles :: (MonadControlIO m, MonadIO m, Applicative m) => String -> Int -> FSDBQuery m [FSProfile]
searchProfiles query maxResults = FSDBQueryC $ do
  let normalizedQuery = map soundexNARA $ splitWs query
  let selector = map ("search_terms" =:) normalizedQuery
  liftIO $ putStrLn $ show selector
  cursor <- find $ select selector "profiles"
  values <- nextN maxResults cursor
  return $ map (fromJust . cast' . Doc) values

saveProfile :: (MonadIO m, Applicative m) => FSProfile -> FSDBQuery m FSProfile
saveProfile profile
  | isJust (profileId profile) = FSDBQueryC $ do
      save "profiles" $ doc
      return profile
  | otherwise = FSDBQueryC $ do
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
postToProfile :: (MonadIO m, Applicative m) => FSPost -> FSObjectId -> FSDBQuery m FSPost
postToProfile post profileId = FSDBQueryC $ do
  modify (select ["_id" =: objId] "profiles") ["$push" =: ["posts" =: post_doc]]
  return post
  where (Doc post_doc) = val post
        objId = toObjectId profileId

data FSPostWithAuthor = FSPostWithAuthor {
  post :: FSPost, author :: FSProfile
  } deriving (Show, Typeable)

postWithAuthor :: (MonadIO m, Applicative m) => FSPost -> FSDBQuery m FSPostWithAuthor
postWithAuthor post = FSDBQueryC $ do
  let (FSDBQueryC _author) = findProfile $ postAuthorId post
  author <- _author
  return $ FSPostWithAuthor post author

{-
 - Friend List Manipulation
 -}

-- Add friend request to the specified user profile
requestFriendship :: (MonadIO m, Applicative m) => FSObjectId -> FSObjectId -> FSDBQuery m FSObjectId
requestFriendship fromUser toUser = FSDBQueryC $ do
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
acceptFriendship :: (MonadIO m) => FSObjectId -> FSObjectId -> FSDBQuery m FSObjectId
acceptFriendship myObjId friendObjId = FSDBQueryC $ do
  modify (select ["_id" =: myId] "profiles") ["$push" =: ["friends" =: friendId]]
  modify (select ["_id" =: friendId] "profiles") ["$push" =: ["friends" =: myId]]
  modify (select ["_id" =: myId] "profiles") ["$pull" =: ["incoming_friend_requests" =: friendId]]
  return friendObjId
  where myId = toObjectId myObjId
        friendId = toObjectId friendObjId

-- Remove friend or friend request
-- XXX: Probably we want to return an error on failure
removeFriendship :: (MonadIO m, Applicative m) => FSObjectId -> FSObjectId -> FSDBQuery m FSObjectId
removeFriendship myObjId friendObjId = FSDBQueryC $ do
  modify (select ["_id" =: myId] "profiles") ["$pull" =: ["incoming_friend_requests" =: friendId]]
  modify (select ["_id" =: myId] "profiles") ["$pull" =: ["friends" =: friendId]]
  modify (select ["_id" =: friendId] "profiles") ["$pull" =: ["friends" =: myId]]
  return friendObjId
  where myId = toObjectId myObjId
        friendId = toObjectId friendObjId


