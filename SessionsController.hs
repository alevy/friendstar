module SessionsController where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import Text.Hastache
import Text.Hastache.Context

import RoutedServer
import Profile

data SessionsController = SessionsController

instance RestController SessionsController where
  -- Renders the login page
  restNew self req = do
    view <- hastacheFile defaultConfig "views/sessions/new.html" $ (\x -> MuNothing)
    return $ mkHtmlResp stat200 $ view

  -- Creates the authentication token and redirects to the user profile page.
  restCreate self req = return $ resp301 "/"

  -- Logs the user out and redirects to the home page
  restDestroy self req = return $ resp301 "/"
