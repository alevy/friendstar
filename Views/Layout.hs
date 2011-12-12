{-# LANGUAGE OverloadedStrings #-}
module Views.Layout where

import Prelude hiding (head, id, div)
import Data.Char
import qualified Data.ByteString.Char8 as S
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (form, label, title)

import Profile

application :: Maybe FSProfile -> Html -> Html
application currentUser content = docTypeHtml $ do
  head $ do
    title "FriendStar"
    link ! rel "stylesheet" ! type_ "text/css" ! href "/style.css"
    script ! src "/javascript/jquery-1.4.2.js" ! type_ "text/javascript" ! charset "utf-8" $ ""
    script ! src "/javascript/application.js" ! type_ "text/javascript" ! charset "utf-8" $ ""
  body $ do
    header $ do
      hgroup $ do
        h1 $ a ! href "/" $ "FriendStar"
        h2 "a social network for people with thoughts"
      nav $ do
        case currentUser of
          Just cu -> do
            ul $ do
              li $ a ! href (toValue $ "/profiles/" ++ (username cu)) $ toHtml $ firstName cu
              li $ a ! href "/friends" $ "Friends"
              li $ a ! href (toValue $ "/profiles/edit/" ++ (username cu)) $ "Edit Profile"
          Nothing -> ""
      form ! action "/search" ! method "POST" ! id "search_form" $ do
        label ! for "search_query" $ "Search:"
        input ! type_ "text" ! name "search[query]" ! id "search_query"
    content