{-# LANGUAGE OverloadedStrings #-}
module WelcomeController where

import Application
import RestController
import RoutedServer
import Profile

data WelcomeController = WelcomeController

instance RestController WelcomeController where

  restIndex self _ = do
    username <- usernameFromSession
    let context = contextFromMUsername username
    renderTemplate "views/welcome/index.html" $ context
