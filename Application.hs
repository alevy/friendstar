{-# LANGUAGE OverloadedStrings #-}
module Application where

import qualified Data.ByteString.Char8 as S

import Text.Hastache

import RestController
import Profile

contextFromMUsername :: Maybe S.ByteString -> MuContext IO
contextFromMUsername username = case username of
                    Just _username -> addGeneric "current_user"
                      (run $ findProfileByUsername _username) $ emptyContext
                    Nothing -> emptyContext
