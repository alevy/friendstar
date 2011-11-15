{-# LANGUAGE OverloadedStrings #-}
module ProfilesController where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IterIO.Iter as I
import Text.Hastache
import Text.Hastache.Context

import Profile
import RestController
import RoutedServer

data ProfilesController = ProfilesController

instance RestController ProfilesController where
  restIndex self = do
    mUser <- usernameFromSession
    case mUser of
      Just user -> render "text/html" $ L.pack $ S.unpack user
      otherwise -> render "text/html" "Not Logged in!"

  restShow self user = do
    profile <- liftIO $ run $ findProfileByUsername user
    renderTemplate "views/profiles/show.html" $ mkGenericContext profile
  
  restEdit self user = do
    profile <- liftIO $ run $ findProfileByUsername user
    renderTemplate "views/profiles/edit.html" $ mkGenericContext profile
  
  restCreate self = do
    req <- getHttpReq
    p <- I.run $ paramMap "profile" req
    let profile = profileFromMap p
    profile <- liftIO $ run $ saveProfile profile
    renderTemplate "views/thankyou.html" $ mkGenericContext profile
  
  restUpdate self user = do
    req <- getHttpReq
    p <- I.run $ paramMap "profile" req
    currentProfile <- liftIO $ run $ findProfileByUsername user
    -- let profile = (profileFromMap p)
    --                   { profileId = profileId currentProfile,
    --                     username = username currentProfile }
    u <- liftIO $ putStrLn $ show p
--    profile <- liftIO $ run $ saveProfile profile
    redirectTo ("/profiles/" ++ "/" ++ (show u))
