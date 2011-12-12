{-# LANGUAGE OverloadedStrings #-}
module PostsController where

import Prelude

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Map ((!))
import Data.Maybe
import qualified Data.Text as T

import RestController
import RoutedServer
import Functions
import Profile

import LIO.LIO (liftLIO)

data PostController = PostController

instance RestController PostController where

  restCreate _ params = do
    mUser <- usernameFromSession
    user <- liftLIO $ run $ findProfileByUsername $ fromJust mUser
    let postMap = paramMap params "post"
    let toUsername = postMap ! "username"
    toUser <- liftLIO $ run $ findProfileByUsername $ S.pack $ L.unpack $ toUsername
    now <- liftLIO $ getCurrentTime
    let newPost = FSPost { postAuthorId = fromJust $ profileId user,
                        postTimestamp = now,
                        postText = T.pack $ L.unpack $ postMap ! "text" }
    _ <- liftLIO $ run $ postToProfile newPost $ fromJust $ profileId toUser
    redirectTo $ "/profiles/" ++ (L.unpack toUsername)

