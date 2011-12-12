{-# LANGUAGE OverloadedStrings #-}
module Application where

import qualified Data.ByteString.Char8 as S

import Text.Hastache
import LIO.DCLabel

import RestController
import Profile

contextFromMUsername :: Maybe S.ByteString -> DC (MuContext IO)
contextFromMUsername _username = case _username of
                    Just _username' -> do
                      curUser <- (run $ findProfileByUsername _username')
                      return $ addGeneric "current_user" curUser emptyContext
                    Nothing -> return emptyContext
