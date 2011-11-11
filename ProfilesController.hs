module ProfilesController where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import Text.StringTemplate
import Text.StringTemplate.GenericStandard

import RoutedServer
import Profile

data ProfilesController = ProfilesController

instance RestController ProfilesController where
  restIndex self req = do
    let template = getTemplate "views/profiles/index.html"
    let view = render $ newSTMP template
    return $ mkHtmlResp stat200 $ view

  restShow self req = do
    let profileId = (head $ reqPathParams req)
    profile <- liftIO $ run $ findProfile (read $ S.unpack profileId)
    let template = getTemplate "views/profile.html"
    let view = render $ setAttribute "profile" profile $
          newSTMP template
    return $ mkHtmlResp stat200 $ view

  restEdit self req = do
    let profileId = (head $ reqPathParams req)
    profile <- liftIO $ run $ findProfile (read $ S.unpack profileId)
    let template = getTemplate "views/edit.html"
    let view = render $ setAttribute "profile" profile $
          newSTMP template
    return $ mkHtmlResp stat200 $ view

  restCreate self req = do
    p <- paramMap "profile" req
    let profile = profileFromMap p
    prof <- liftIO $ run $ saveProfile profile
    let template = getTemplate "views/thankyou.html"
    let view = render $ setAttribute "profile" prof $
          newSTMP template
    return $ mkHtmlResp stat200 $ view

  restUpdate self req = do
    p <- paramMap "profile" req
    let profileId = head $ reqPathParams req
    let profile = (profileFromMap p) { profileId = Just $ profileId }
    liftIO $ run $ saveProfile profile
    return $ resp301 ("/profiles/" ++ S.unpack profileId)
