{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Blob where

import Control.Monad.Trans
import qualified Data.ByteString as S
import Data.Maybe
import Database.MongoDB
import Data.Typeable

import FSDB

data FSBlob = FSBlob {
  blobId :: Maybe FSObjectId,
  blobData :: S.ByteString,
  blobMime :: S.ByteString
} deriving (Typeable, Show, Eq)

instance Val FSBlob where
  val blob = Doc [ "_id" =: blobId blob,
                   "mime" =: blobMime blob,
                   "data" =: blobData blob]

getBlob :: MonadIO m => FSObjectId -> Action m FSBlob
getBlob blobId = fetch $ select ["_id" =: blobId] "blobs"

saveBlob :: MonadIO m => FSBlob -> Action m FSBlob
saveBlob blob 
  | isJust (blobId blob) = do
      save "blobs" $ doc
      return blob
  | otherwise = do
      newId <- insert "blobs" $ exclude ["_id"] doc
      return blob { blobId = newId }
  where (Doc doc) = val blob

