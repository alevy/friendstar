module ProfilesController where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import Text.Hastache
import Text.Hastache.Context

import Profile
import RoutedServer

data ProfilesController = ProfilesController

instance RestController ProfilesController where
  restIndex self req = do
    view <- hastacheFile defaultConfig "views/profiles/index.html" context
    return $ mkHtmlResp stat200 $ view
    where context _ = MuNothing

  restShow self req = do
    let profileId = (head $ reqPathParams req)
    profile <- liftIO $ run $ findProfile (read $ S.unpack profileId)
    view <- hastacheFile defaultConfig "views/profiles/show.html" $
                    mkGenericContext profile
    return $ mkHtmlResp stat200 $ view

  restEdit self req = do
    let profileId = head $ reqPathParams req
    profile <- liftIO $ run $ findProfile (read $ S.unpack profileId)
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
    let profileId = head $ reqPathParams req
    currentProfile <- liftIO $ run $ findProfile (read $ S.unpack profileId)
    let profile = (profileFromMap p) { profileId = Just $ profileId, username = username currentProfile }
    liftIO $ run $ saveProfile profile
    return $ resp301 ("/profiles/" ++ S.unpack profileId)
