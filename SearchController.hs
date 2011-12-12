{-# LANGUAGE OverloadedStrings #-}
module SearchController where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe
import LIO.LIO (liftLIO)

import Application
import RestController
import Profile

data SearchController = SearchController

instance RestController SearchController where
  -- Searches for users by username, name and city
  restCreate _ params = do
    _username <- usernameFromSession
    context <- liftLIO $ contextFromMUsername _username
    let query = L.unpack $ fst $ fromJust $ lookup "search[query]" params
    profiles <- liftLIO $ run $ searchProfiles query 10
    renderTemplate "views/search/search.html" $ addVar "query" query $
      addGenericList "profiles" profiles $ context
