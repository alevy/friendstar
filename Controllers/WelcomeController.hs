{-# LANGUAGE OverloadedStrings #-}
module Controllers.WelcomeController where

import Application
import RestController

import Views.Welcome

data WelcomeController = WelcomeController

instance RestController WelcomeController where

  restIndex _ _ = do
    cu <- currentUser
    renderTemplate $ index cu
