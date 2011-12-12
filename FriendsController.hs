{-# LANGUAGE OverloadedStrings #-}
module FriendsController where

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe
import Text.Hastache
import LIO.LIO (liftLIO)

import Application
import RestController
import Profile

data FriendsController = FriendsController

instance RestController FriendsController where
  -- List friends and friend requests
  restIndex _ _ = do
    mUser <- usernameFromSession
    context <- liftLIO $ contextFromMUsername mUser
    profile <- liftLIO $ run $ findProfileByUsername $ fromJust mUser
    friendList <- liftLIO $ run $ mapM (findProfile) (friends profile)
    friendReqList <- liftLIO $ run $ mapM (findProfile) (incomingFriendRequests profile)
    let expandedProfile = addVar "firstName" (firstName profile) $ addVar "lastName" (lastName profile) $ addGenericList "friends" friendList $ addGenericList "friendRequests" friendReqList $ context
    renderTemplate "views/friends/index.html" $ expandedProfile

  -- List friends of a user
  restShow _ user _ = do
    mUser <- usernameFromSession
    context <- liftLIO $ contextFromMUsername mUser
    profile <- liftLIO $ run $ findProfileByUsername $ user
    friendList <- liftLIO $ run $ mapM (findProfile) (friends profile)
    let expandedProfile = addVar "firstName" (firstName profile) $ addVar "lastName" (lastName profile) $ addGenericList "friends" friendList $ context
    renderTemplate "views/friends/show.html" $ expandedProfile

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

  -- Friend request form
  restNew _ _ = do
    renderTemplate "views/friends/new.html" $ (\_ -> MuNothing)

  -- Add friend request
  restCreate _ params = do
    mUser <- usernameFromSession
    user <- liftLIO $ run $ findProfileByUsername $ fromJust mUser
    let reqUser = S.pack $ L.unpack $ fst $ fromJust $ lookup "friend[username]" params
    fUser <- liftLIO $ run $ findProfileByUsername $ reqUser
    _ <- return $ run $ requestFriendship (fromJust $ profileId user) (fromJust $ profileId fUser)
    redirectTo "/friends/"

