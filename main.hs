{-# LANGUAGE OverloadedStrings #-}

import Profile

import Control.Monad.Trans

import RoutedServer
import qualified Data.ByteString.Char8 as S
import Text.StringTemplate
import Text.StringTemplate.GenericStandard

import System.IO

main :: IO ()
main = do
  runHttpServer 8000 routing

routing = [ routeTop $ routeConst $ resp301 "/home",
                    routeName "profiles" $ routeRestController (ProfilesController 1),
                    routeFileSys mimeMap (dirRedir "/index.html") "public"
                  ]

data ProfilesController = ProfilesController Integer

instance RestController ProfilesController where
  restIndex self req = do
    let template = getTemplate "views/profiles/index.html"
    let view = render $ newSTMP template
    return $ mkHtmlResp stat200 $ view
  
  restShow self req = do
    let profileId = (head $ reqPathParams req)
    profile <- lift $ run $ findProfile (read $ S.unpack profileId)
    let template = getTemplate "views/profile.html"
    let view = render $ setAttribute "profile" profile $
          newSTMP template
    return $ mkHtmlResp stat200 $ view

  restEdit self req = do
    let profileId = (head $ reqPathParams req)
    profile <- lift $ run $ findProfile (read $ S.unpack profileId)
    let template = getTemplate "views/edit.html"
    let view = render $ setAttribute "profile" profile $
          newSTMP template
    return $ mkHtmlResp stat200 $ view
