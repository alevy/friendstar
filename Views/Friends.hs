{-# LANGUAGE OverloadedStrings #-}
module Views.Friends where

import Prelude hiding (head, id, div)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes

import Profile

listFriends :: [FSProfile] -> Html
listFriends friends = ol $ mapM_ showFriend friends
  where showFriend profile = li $ a ! href (url profile) $ toHtml $ fullName profile
        url profile = toValue $ "/profiles/" ++ username profile

index :: [FSProfile] -> [FSProfile] -> Html
index friendRequests friends = do
  div ! id "friendRequests" $ do
    h2 $ "Friend Requests"
    if length friendRequests == 0 then
      p "No pending friend requests."
    else
      listFriends friendRequests
  div ! id "friends" $ do
    h2 $ "Friends"
    if length friends == 0 then
      p "You have zero friends. Use the search bar above to find some!"
    else
      listFriends friends
      