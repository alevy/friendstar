{-# LANGUAGE OverloadedStrings #-}
module ProfilesController where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IterIO.Iter as I (run)
import Data.IterIO.Inum
import Data.IterIO.Http
import Text.Hastache.Context

import Application
import Profile
import RestController
import RoutedServer

data ProfilesController = ProfilesController

instance RestController ProfilesController where
  restIndex self _ = do
    mUser <- usernameFromSession
    case mUser of
      Just user -> redirectTo ("/profiles/" ++ (S.unpack user))
      otherwise -> redirectTo ("/")

  restShow self user _ = do
    context <- contextFromMUsername `fmap` usernameFromSession
    let profile = run $ findProfileByUsername user
    renderTemplate "views/profiles/show.html" $ addVar "profile" (123::Integer) $ context
  
  restEdit self user _ = do
    let profile = run $ findProfileByUsername user
    renderTemplate "views/profiles/edit.html" $ mkGenericContext profile
  
  restCreate self params = do
    req <- getHttpReq
    let profile = profileFromMap $ paramMap params "profile"
    let profile = run $ saveProfile profile
    renderTemplate "views/thankyou.html" $ mkGenericContext profile
  
  restUpdate self user params = do
    req <- getHttpReq
    let p = paramMap params "profile"
    let currentProfile = run $ findProfileByUsername user
    let profile = (profileFromMap p)
                      { profileId = profileId currentProfile,
                        username = username currentProfile }
    u <- liftIO $ putStrLn $ show params
    let profile = run $ saveProfile profile
    redirectTo ("/profiles/" ++ (username profile))
