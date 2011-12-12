{-# LANGUAGE OverloadedStrings #-}
module Controllers.WelcomeController where

import Application
import RestController

import Views.Welcome

data WelcomeController = WelcomeController

instance RestController WelcomeController where

  restIndex _ _ = do
    _username <- usernameFromSession
    renderTemplate $ index _username
