{-# LANGUAGE OverloadedStrings #-}
module SearchController where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe

import Application
import RestController
import RoutedServer
import Profile

data SearchController = SearchController

instance RestController SearchController where
  -- Searches for users by username, name and city
  restCreate self params = do
    context <- contextFromMUsername `fmap` usernameFromSession
    let query = L.unpack $ fst $ fromJust $ lookup "search[query]" params
    let profiles = run $ searchProfiles query 10
    renderTemplate "views/search/search.html" $ addVar "query" query $
      addGenericList "profiles" profiles $ context
