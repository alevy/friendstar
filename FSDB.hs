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

runM :: FSDBQuery IO a -> DC (Maybe a)
runM (FSDBQueryC act) = ioTCB $ do
  pipe <- server
  acc <- access pipe master "friendstar" act
  case acc of
    (Right result) -> return $ Just result
    (Left f) -> return Nothing

run :: FSDBQuery IO a -> DC a
run (FSDBQueryC act) = ioTCB $ do
  pipe <- server
  acc <- access pipe master "friendstar" act
  case acc of
    (Right result) -> return result
    (Left f) -> do
      putStrLn $ show f
      return undefined

type FSObjectId = S.ByteString

fromObjectId :: ObjectId -> FSObjectId
fromObjectId _id = S.pack $ show _id

toObjectId :: FSObjectId -> ObjectId
toObjectId _id = read $ S.unpack _id

