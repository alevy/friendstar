{-# LANGUAGE OverloadedStrings #-}
module FriendsController where

import System.IO.Unsafe
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

import Application
import RestController
import RoutedServer
import Profile

data FriendsController = FriendsController

instance RestController FriendsController where
  -- List friends and friend requests
  restIndex self _ = do
    mUser <- usernameFromSession
    context <- contextFromMUsername `fmap` usernameFromSession
    let profile = run $ findProfileByUsername $ fromJust mUser
    let friendList = run $ mapM (findProfile) (friends profile)
    let friendReqList = run $ mapM (findProfile) (incomingFriendRequests profile)
    let expandedProfile = addVar "firstName" (firstName profile) $ addVar "lastName" (lastName profile) $ addGenericList "friends" friendList $ addGenericList "friendRequests" friendReqList $ context
    renderTemplate "views/friends/index.html" $ expandedProfile

  -- List friends of a user
  restShow self user _ = do
    context <- contextFromMUsername `fmap` usernameFromSession
    let profile = run $ findProfileByUsername $ user
    let friendList = run $ mapM (findProfile) (friends profile)
    let expandedProfile = addVar "firstName" (firstName profile) $ addVar "lastName" (lastName profile) $ addGenericList "friends" friendList $ context
    renderTemplate "views/friends/show.html" $ expandedProfile

  -- Accept friend request
  restUpdate self user _ = do
    mUser <- usernameFromSession
    let profile = run $ findProfileByUsername $ fromJust mUser
    let friendProfile = run $ findProfileByUsername user
    if (friendshipRequestExists profile friendProfile) then do
      return $ run $
        acceptFriendship (fromJust $ profileId profile)
                        (fromJust $ profileId friendProfile)
      redirectTo "/friends/"
    else
      render "text/html" "Friendship request not present!"

  -- Remove a friend or request
  restDestroy self user _ = do
    mUser <- usernameFromSession
    let profile = run $ findProfileByUsername $ fromJust mUser
    let friendProfile = run $ findProfileByUsername user
    return $ run $ removeFriendship (fromJust $ profileId profile) (fromJust $ profileId friendProfile)
    redirectTo "/friends/"

  -- Friend request form
  restNew self _ = do
    renderTemplate "views/friends/new.html" $ (\_ -> MuNothing)

  -- Add friend request
  restCreate self params = do
    mUser <- usernameFromSession
    let user = run $ findProfileByUsername $ fromJust mUser
    let reqUser = S.pack $ L.unpack $ fromJust $ lookup "friend[username]" params
    let fUser = run $ findProfileByUsername $ reqUser
    return $ run $ requestFriendship (fromJust $ profileId user) (fromJust $ profileId fUser)
    redirectTo "/friends/"

