module FSDB where

import qualified Data.ByteString.Char8 as S
import Database.MongoDB

type FSObjectId = S.ByteString

fromObjectId :: ObjectId -> FSObjectId
fromObjectId id = S.pack $ show id

toObjectId :: FSObjectId -> ObjectId
toObjectId id = read $ S.unpack id

