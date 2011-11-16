{-# LANGUAGE OverloadedStrings #-}
module FriendsController where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IterIO.Iter as I (run)
import Data.Maybe
import qualified Data.Text as T
import Data.IterIO.Inum
import Data.IterIO.Http
import Text.Hastache
import Text.Hastache.Context

import RestController
import RoutedServer
import Profile

data FriendsController = FriendsController

instance RestController FriendsController where
  -- List friends
  restIndex self _ = do
    mUser <- usernameFromSession
    user <- liftIO $ run $ findProfileByUsername $ fromJust mUser
    render "text/html" $ L.pack $ show (friends user) ++ show (incomingFriendRequests user)

  -- List friends of a user
  restShow self user _ = do
    mUser <- usernameFromSession
    profile <- liftIO $ run $ findProfileByUsername $ fromJust mUser
    renderTemplate "views/friends/show.html" $ mkGenericContext profile

  -- Accept friend request
  --restUpdate self user params = do

  -- Remove a friend or request
  --restDestroy

  -- Friend request form
  restNew self _ = do
    renderTemplate "views/friends/new.html" $ (\_ -> MuNothing)

  -- Add friend request
  restCreate self params = do
    mUser <- usernameFromSession
    user <- liftIO $ run $ findProfileByUsername $ fromJust mUser
    let reqUser = S.pack $ L.unpack $ fromJust $ lookup "friend[username]" params
    fUser <- liftIO $ run $ findProfileByUsername $ reqUser
    liftIO $ run $ requestFriendship (fromJust $ profileId user) (fromJust $ profileId fUser)
    render "text/html" $ L.pack $ show (username fUser)

