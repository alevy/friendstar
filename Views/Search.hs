{-# LANGUAGE OverloadedStrings #-}
module Views.Search where

import Prelude hiding (head, id, div)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (form, label)

import Application
import Profile

search :: String -> [FSProfile] -> Html
search query profiles = do
  h2 $ do "Search results for "; em $ toHtml query
  if length profiles > 0 then
    ol ! class_ "results" $ mapM_ (\p ->
      li $ do
        a ! href (toValue $ "/profiles/" ++ (username p)) $ toHtml $ fullName p
        em $ toHtml $ currentCity p) profiles
  else
    ""