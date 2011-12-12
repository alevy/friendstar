{-# LANGUAGE OverloadedStrings #-}
module Controllers.SearchController where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe
import LIO.LIO (liftLIO)

import Application
import RestController
import Profile

import Views.Search

data SearchController = SearchController

instance RestController SearchController where
  -- Searches for users by username, name and city
  restCreate _ params = do
    let query = L.unpack $ fst $ fromJust $ lookup "search[query]" params
    profiles <- liftLIO $ run $ searchProfiles query 10
    renderTemplate $ search query profiles
