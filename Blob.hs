{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Blob where

import Control.Applicative
import Control.Monad.Trans
import qualified Data.ByteString as S
import Data.Maybe
import Database.MongoDB
import Data.Typeable

import FSDB

data FSBlob = FSBlob {
  blobId :: Maybe FSObjectId,
  blobData :: S.ByteString,
  blobMimeType :: String
} deriving (Typeable, Show, Eq)

bsFromBinary :: Binary -> S.ByteString
bsFromBinary (Binary bs) = bs

instance Val FSBlob where
  val blob = Doc [ "_id" =: (fmap toObjectId $ blobId blob),
                   "mime_type" =: (blobMimeType blob),
                   "data" =: (Binary $ blobData blob)]
  cast' (Doc doc) = Just FSBlob {
    blobId = fmap fromObjectId $ at "_id" doc,
    blobData = (bsFromBinary $ at "data" doc),
    blobMimeType = (at "mime_type" doc)
  }
  
  cast' _ = fail "Cannot convert type to FSBlob"

getBlob :: MonadIO m => FSObjectId -> Action m FSBlob
getBlob blbId = do
  dbBlob <- fetch $ select ["_id" =: toObjectId blbId] "blobs"
  let (Just blob) = cast' (Doc dbBlob)
  return blob

saveBlob :: (MonadIO m, Applicative m) => FSBlob -> Action m FSBlob
saveBlob blob 
  | isJust (blobId blob) = do
      save "blobs" $ doc
      return blob
  | otherwise = do
      newId <- insert "blobs" $ exclude ["_id"] doc
      return blob { blobId = fmap fromObjectId $ cast' newId }
  where (Doc doc) = val blob

