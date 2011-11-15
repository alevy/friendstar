
module SessionsController where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Map
import qualified Data.IterIO.Iter as I
import Text.Hastache
import Text.Hastache.Context
import Web.ClientSession

import RestController
import RoutedServer
import Profile

data SessionsController = SessionsController

cookieKey = initKey $
            S.pack "9d3583564c8965d36e5f75c5cb8b1323373545cc0fe98aab170d2a155a93c16e6b6d4703c216a8e99691a09ed25cd440"


cookieFromString str = liftIO $ fmap S.unpack $ encryptIO key str
  where (Right key) = cookieKey
instance RestController SessionsController where
  -- Renders the login page
  restNew self = do
    renderTemplate "views/sessions/new.html" $ (\x -> MuNothing)

  -- Creates the authentication token and redirects to the user profile page.
  restCreate self = do
    req <- getHttpReq
    params <- I.run $ paramMap "session" req
    let username = S.pack $ L.unpack $ params ! "username"
    -- TODO: INSECURE!!! For now just store username because ClientSession leaves a trailing `=' which is invalid.
    setSession $ S.unpack username
    redirectTo "/"

  -- Logs the user out and redirects to the home page
  restDestroy self arg = do
    destroySession
    redirectTo "/"
