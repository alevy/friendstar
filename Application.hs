{-# LANGUAGE OverloadedStrings #-}
module Application where

import qualified Data.ByteString.Char8 as S

import RestController
import Profile

--contextFromMUsername ::   
contextFromMUsername username = case username of
                    Just _username -> addGeneric "current_user"
                      (run $ findProfileByUsername _username) $ emptyContext
                    Nothing -> emptyContext