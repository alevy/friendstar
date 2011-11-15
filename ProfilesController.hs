{-# LANGUAGE OverloadedStrings #-}
module ProfilesController where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Text.Hastache
import Text.Hastache.Context

import Profile
import RoutedServer

data ProfilesController = ProfilesController

instance RestController ProfilesController where
  restIndex self = do
    username <- usernameFromSession
    return $ mkHtmlResp stat200 $ L.pack $ (show username)
    where context _ = MuNothing

  restShow self req = do
    let username = (head $ reqPathParams req)
    profile <- liftIO $ run $ findProfileByUsername username
    view <- hastacheFile defaultConfig "views/profiles/show.html" $
                    mkGenericContext profile
    return $ mkHtmlResp stat200 $ view

  restEdit self req = do
    let username = head $ reqPathParams req
    profile <- liftIO $ run $ findProfileByUsername username
    view <- hastacheFile defaultConfig "views/profiles/edit.html" $
                    mkGenericContext profile
    return $ mkHtmlResp stat200 $ view

  restCreate self req = do
    p <- paramMap "profile" req
    let profile = profileFromMap p
    profile <- liftIO $ run $ saveProfile profile
    view <- hastacheFile defaultConfig "views/thankyou.html" $
                    mkGenericContext profile
    return $ mkHtmlResp stat200 $ view

  restUpdate self req = do
    p <- paramMap "profile" req
    let user = head $ reqPathParams req
    currentProfile <- liftIO $ run $ findProfileByUsername user
    let profile = (profileFromMap p)
                    { profileId = profileId currentProfile,
                      username = username currentProfile }
    liftIO $ run $ saveProfile profile
    return $ resp301 ("/profiles/" ++ S.unpack user)
