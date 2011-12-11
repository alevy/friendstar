{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module FSDB where

import qualified Data.ByteString.Char8 as S
import Database.MongoDB

import LIO.TCB
import LIO.DCLabel

newtype FSDBQuery m a = FSDBQueryC (Action m a)
  deriving (Monad)

server :: IO Pipe
server = runIOE $ connect $ host "127.0.0.1"

run :: FSDBQuery IO a -> DC a
run (FSDBQueryC act) = ioTCB $ do
  pipe <- server
  (Right result) <- access pipe master "friendstar" act
  return result

type FSObjectId = S.ByteString

fromObjectId :: ObjectId -> FSObjectId
fromObjectId _id = S.pack $ show _id

toObjectId :: FSObjectId -> ObjectId
toObjectId _id = read $ S.unpack _id

