{-# LANGUAGE OverloadedStrings #-}
module ProfilesController where

import qualified Data.ByteString.Char8 as S
import Data.Foldable
import LIO.LIO (liftLIO)
import LIO.DCLabel

import Application
import Profile
import RestController
import RoutedServer

data ProfilesController = ProfilesController

defaultContext = do
  _username <- usernameFromSession
  liftLIO $ contextFromMUsername _username

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
    context <- defaultContext
    profile <- liftLIO $ run $ findProfileByUsername user
    profilePosts <- liftLIO $ postsToPostsWithAuthor $ take 10 $ posts profile
    renderTemplate "views/profiles/show.html" $ addGenericList "posts" profilePosts $ addGeneric "profile" profile $ context
  
  restEdit _ user _ = do
    context <- defaultContext
    profile <- liftLIO $ run $ findProfileByUsername user
    renderTemplate "views/profiles/edit.html" $ addGeneric "profile" profile $ context
  
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
