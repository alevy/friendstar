{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Profile where

import Prelude hiding (lookup)

import Control.Applicative
import Control.Monad.Trans
import Data.Bson
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
  profileId = Nothing, firstName = "", middleName = Nothing, lastName = "",
  currentCity = Nothing, friends = [], incomingFriendRequests = [], posts = []
}

profileFromMap :: Map String L.ByteString -> FSProfile
profileFromMap map = defaultFSProfile {
  firstName = L.unpack $ map Data.Map.! "first_name",
  lastName = L.unpack $ map Data.Map.! "last_name",
  currentCity = fmap L.unpack $ "current_city" `Data.Map.lookup` map
}

instance Val FSProfile where
  val profile = Doc [ "_id" =: (fmap toObjectId $ profileId profile),
                  "first_name" =: firstName profile,
                  "middle_name" =: middleName profile,
                  "last_name" =: lastName profile,
                  "current_city" =: currentCity profile,
                  "friends" =: (fmap toObjectId $ friends profile),
                  "friends" =: (fmap toObjectId $ friends profile),
                  "incoming_friend_requests" =: (fmap toObjectId $ incomingFriendRequests profile),
                  "posts" =: posts profile]

  cast' (Doc doc) = Just defaultFSProfile {
    profileId = fmap fromObjectId $ at "_id" doc,
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

findProfile :: MonadIO m => ObjectId -> Action m FSProfile
findProfile id = do
  dbProfile <- fetch (select ["_id" =: id] "profiles")
  let (Just profile) = cast' (Doc dbProfile)
  return profile

saveProfile :: (MonadIO m, Applicative m) => FSProfile -> Action m FSProfile
saveProfile profile
  | isJust (profileId profile) = do
      save "profiles" $ doc
      return profile
  | otherwise = do
      ObjId objId <- Database.MongoDB.insert "profiles" $ exclude ["_id"] doc
      return profile { profileId = Just $ fromObjectId objId }
  where (Doc doc) = val profile


run act = do
  pipe <- runIOE $ connect $ host "127.0.0.1"
  (Right profiles) <- access pipe master "friendstar" act
  return profiles

