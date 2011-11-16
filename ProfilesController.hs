{-# LANGUAGE OverloadedStrings #-}
module ProfilesController where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IterIO.Iter as I (run)
import Data.IterIO.Inum
import Data.IterIO.Http
import Text.Hastache.Context

import Profile
import RestController
import RoutedServer

data ProfilesController = ProfilesController

instance RestController ProfilesController where
  restIndex self _ = do
    mUser <- usernameFromSession
    case mUser of
      Just user -> redirectTo ("/profiles/" ++ (S.unpack user))
      otherwise -> render "text/html" "Not Logged in!"

  restShow self user _ = do
    profile <- liftIO $ run $ findProfileByUsername user
    renderTemplate "views/profiles/show.html" $ mkGenericContext profile
  
  restEdit self user _ = do
    profile <- liftIO $ run $ findProfileByUsername user
    renderTemplate "views/profiles/edit.html" $ mkGenericContext profile
  
  restCreate self params = do
    req <- getHttpReq
    let profile = profileFromMap $ paramMap params "profile"
    profile <- liftIO $ run $ saveProfile profile
    renderTemplate "views/thankyou.html" $ mkGenericContext profile
  
  restUpdate self user params = do
    req <- getHttpReq
    let p = paramMap params "profile"
    currentProfile <- liftIO $ run $ findProfileByUsername user
    let profile = (profileFromMap p)
                      { profileId = profileId currentProfile,
                        username = username currentProfile }
    u <- liftIO $ putStrLn $ show params
    profile <- liftIO $ run $ saveProfile profile
    redirectTo ("/profiles/" ++ (username profile))
