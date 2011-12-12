{-# LANGUAGE OverloadedStrings #-}
module ProfilesController where

import Prelude hiding (show)
import qualified Data.ByteString.Char8 as S
import Data.Foldable
import LIO.LIO (liftLIO)
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
    profilePosts <- liftLIO $ postsToPostsWithAuthor $ take 10 $ posts profile
    renderTemplate $ show profile profilePosts
  
  restEdit _ user _ = do
    profile <- liftLIO $ run $ findProfileByUsername user
    renderTemplate $ edit profile
  
  restCreate _ params = do
    let profile = profileFromMap $ paramMap params "profile"
    _ <- return $ run $ saveProfile profile
    redirectTo $ "/profiles/" ++ (username profile)
    setSession (username profile)
  
  restUpdate _ user params = do
    let p = paramMap params "profile"
    currentProfile <- liftLIO $ run $ findProfileByUsername user
    let profile = (profileFromMap p)
                      { profileId = profileId currentProfile,
                        username = username currentProfile }
    profile' <- liftLIO $ run $ saveProfile profile
    redirectTo ("/profiles/" ++ (username profile'))
