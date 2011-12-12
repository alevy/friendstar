{-# LANGUAGE OverloadedStrings #-}
module Controllers.FriendsController where

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe
import LIO.LIO (liftLIO)

import Application
import RestController
import Profile

import Views.Friends

data FriendsController = FriendsController

instance RestController FriendsController where
  -- List friends and friend requests
  restIndex _ _ = do
    mUser <- usernameFromSession
    profile <- liftLIO $ run $ findProfileByUsername $ fromJust mUser
    friendsL <- liftLIO $ run $ mapM (findProfile) (friends profile)
    friendRequests <- liftLIO $ run $ mapM (findProfile) (incomingFriendRequests profile)
    renderTemplate $ index friendRequests friendsL

  -- Accept friend request
  restUpdate _ user _ = do
    mUser <- usernameFromSession
    profile <- liftLIO $ run $ findProfileByUsername $ fromJust mUser
    friendProfile <- liftLIO $ run $ findProfileByUsername user
    if (friendshipRequestExists profile friendProfile) then do
      _ <- return $ run $
        acceptFriendship (fromJust $ profileId profile)
                        (fromJust $ profileId friendProfile)
      redirectTo "/friends/"
    else
      render "text/html" "Friendship request not present!"

  -- Remove a friend or request
  restDestroy _ user _ = do
    mUser <- usernameFromSession
    profile <- liftLIO $ run $ findProfileByUsername $ fromJust mUser
    friendProfile <- liftLIO $ run $ findProfileByUsername user
    _ <- return $ run $ removeFriendship (fromJust $ profileId profile) (fromJust $ profileId friendProfile)
    redirectTo "/friends/"

  -- Add friend request
  restCreate _ params = do
    mUser <- usernameFromSession
    user <- liftLIO $ run $ findProfileByUsername $ fromJust mUser
    let reqUser = S.pack $ L.unpack $ fst $ fromJust $ lookup "friend[username]" params
    fUser <- liftLIO $ run $ findProfileByUsername $ reqUser
    _ <- return $ run $ requestFriendship (fromJust $ profileId user) (fromJust $ profileId fUser)
    redirectTo "/friends/"

