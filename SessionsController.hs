
module SessionsController where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Map
import Text.Hastache
import Text.Hastache.Context
import Web.ClientSession

import RoutedServer
import Profile

data SessionsController = SessionsController

cookieKey = key
  where (Right key) = initKey $
    S.pack "9d3583564c8965d36e5f75c5cb8b1323373545cc0fe98aab170d2a155a93c16e6b6d4703c216a8e99691a09ed25cd440"


cookieFromString str = liftIO $ fmap S.unpack $ encryptIO key str
  where (Right key) = cookieKey
instance RestController SessionsController where
  -- Renders the login page
  restNew self req = do
    view <- hastacheFile defaultConfig "views/sessions/new.html" $ (\x -> MuNothing)
    return $ mkHtmlResp stat200 $ view

  -- Creates the authentication token and redirects to the user profile page.
  restCreate self req = do
    params <- paramMap "session" req
    let username = S.pack $ L.unpack $ params ! "username"
    cookie <- cookieFromString username
    let cookieHeader = S.pack $ "Set-Cookie: _sess=" ++ cookie ++ "; path=/;"
    let resp = resp301 "/"
    return $ resp { respHeaders = cookieHeader:(respHeaders resp)}

  -- Logs the user out and redirects to the home page
  restDestroy self req = do
    let cookieHeader = S.pack $ "Set-Cookie: _sess=; path=/; expires=Thu, Jan 01 1970 00:00:00 UTC;"
    let resp = resp301 "/"
    return $ resp { respHeaders = cookieHeader:(respHeaders resp)}
