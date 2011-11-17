{-# LANGUAGE OverloadedStrings #-}
module PostsController where

import Prelude

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Map ((!))
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock
import Text.Hastache

import RestController
import RoutedServer
import Profile

data PostController = PostController

instance RestController PostController where

  restNew self _ = do
    renderTemplate "views/posts/new.html" $ (\_ -> MuNothing)

  restCreate self params = do
    mUser <- usernameFromSession
    let user = run $ findProfileByUsername $ fromJust mUser
    let mProfileUsername = S.pack $ L.unpack $ fromJust $ lookup "profile[username]" params
    let profile = run $ findProfileByUsername mProfileUsername
    let postMap = paramMap params "post"
    now <- liftIO $ getCurrentTime
    let post = FSPost { postAuthorId = fromJust $ profileId user,
                        postTimestamp = now,
                        postText = T.pack $ L.unpack $ postMap ! "text" }
    return $ run $ postToProfile post $ fromJust $ profileId user
    render "text/html" $ L.pack $ show post

