{-# LANGUAGE OverloadedStrings #-}
module Controllers.ProfilesController where

import Prelude hiding (show)
import qualified Data.ByteString.Char8 as S
import Data.Maybe
import Data.Foldable
import LIO.LIO (liftLIO, unlabel)
import LIO.DCLabel

import Application
import Profile
import RestController
import RoutedServer

import Views.Profiles

data ProfilesController = ProfilesController

postsToPostsWithAuthor :: [FSPost] -> DC [FSPostWithAuthor]
postsToPostsWithAuthor = foldrM go []
  where go p accm = do
          result <- liftLIO $ run . postWithAuthor $ p
          return $ result:accm

instance RestController ProfilesController where
  restIndex _ _ = do
    mUser <- usernameFromSession
    case mUser of
      Just user -> redirectTo ("/profiles/" ++ (S.unpack user))
      Nothing -> redirectTo ("/")

  restShow _ user _ = do
    profile <- liftLIO $ run $ findProfileByUsername user
    mcurrentCity <- liftLIO $ unlabel $ currentCity profile
    profilePosts <- liftLIO $ postsToPostsWithAuthor $ take 10 $ posts profile
    renderTemplate $ show profile profilePosts mcurrentCity
  
  restEdit _ user _ = do
    profile <- liftLIO $ run $ findProfileByUsername user
    mcurrentCity <- liftLIO $ unlabel $ currentCity profile
    renderTemplate $ edit profile mcurrentCity
  
  restCreate _ params = do
    user <- fmap (S.unpack . fromJust) usernameFromSession
    let profile = profileFromMap user $ paramMap params "profile"
    _ <- liftLIO $ run $ saveProfile profile
    redirectTo $ "/profiles/" ++ (username profile)
    setSession (username profile)
  
  restUpdate _ user params = do
    mUser <- fmap fromJust usernameFromSession
    let p = paramMap params "profile"
    currentProfile <- liftLIO $ run $ findProfileByUsername mUser
    let profile = (profileFromMap "" p)
                      { profileId = profileId currentProfile,
                        username = username currentProfile }
    profile' <- liftLIO $ run $ saveProfile profile
    redirectTo ("/profiles/" ++ (username profile'))
