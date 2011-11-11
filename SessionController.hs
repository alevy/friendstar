module SessionController where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import Text.StringTemplate
import Text.StringTemplate.GenericStandard

import RoutedServer
import Profile

data SessionController = SessionController

instance RestController SessionController where
  -- Renders the login page
  restNew self req = return $ resp404 req

  -- Creates the authentication token and redirects to the user profile page.
  restCreate self req = return $ resp404 req

  -- Logs the user out and redirects to the home page
  restDestroy self req = return $ resp404 req
