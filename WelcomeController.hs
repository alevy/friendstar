{-# LANGUAGE OverloadedStrings #-}
module WelcomeController where

import LIO.LIO (liftLIO)

import Application
import RestController
import RoutedServer
import Profile

data WelcomeController = WelcomeController

instance RestController WelcomeController where

  restIndex self _ = do
    _username <- usernameFromSession
    context <- liftLIO $ contextFromMUsername _username
    renderTemplate "views/welcome/index.html" $ context
