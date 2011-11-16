{-# LANGUAGE OverloadedStrings #-}
module FriendsController where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IterIO.Iter as I (run)
import Data.IterIO.Inum
import Data.IterIO.Http
import Text.Hastache
import Text.Hastache.Context

import Profile
import RestController
import RoutedServer

data ProfilesController = FriendsController

instance RestController FriendsController where
  -- List friends
  restIndex self _ = do
    mUser <- usernameFromSession
    user <- liftIO $ run $ findProfileByUsername $ fromJust mUser
    render "text/html" $ show $ friends user
{-
  restShow self user _ = do
  restEdit self user _ = do
  restCreate self params = do
  restUpdate self user params = do
-}
