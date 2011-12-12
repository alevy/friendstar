{-# LANGUAGE OverloadedStrings #-}
module Views.Profiles where

import Prelude hiding (head, id, div)
import Data.Char
import qualified Data.ByteString.Char8 as S
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (form, label)

import Application
import Profile

field :: ToValue a => String -> Bool -> a -> Html
field _name req def = div ! class_ "field" $ do
  label ! for idName $
    if req then do
      toHtml $ ((capitalize _name) ++ ":")
      sup "*"
    else do
      toHtml $ ((capitalize _name) ++ ":")
  input ! type_ "text" ! name (toValue $ "profile[" ++ _name ++ "]") ! id idName ! value (toValue def)
  where capitalize (x:xs) = toUpper x : xs
        capitalize [] = []
        idName = toValue $ ("profile_" ++ _name)

show :: FSProfile -> [FSPostWithAuthor] -> Maybe String -> Html
show profile posts mcurrentCity = do
  h2 $ toHtml $ fullName profile
  case profilePicId profile of
    Just profilePId ->
      div ! id "profile_pic" $ img ! src (toValue $ "/profile_pics/" ++ (S.unpack profilePId))
    Nothing -> ""
  div ! id "details" $ do
    "Current City: "
    toHtml $ mcurrentCity
  div ! id "posts" $ do
    h3 "Wall"
    form ! action "/posts/" ! method "POST" $ do
      input ! type_ "hidden" ! name "post[username]" ! value (toValue $ username profile)
      div ! class_ "field" $ textarea ! name "post[text]" $ ""
      div $ input ! type_ "submit" ! value "Post"
    if length posts == 0 then
      "No posts yet"
    else
      ol ! id "posts" $ mapM_ (\postA -> do
        li $ do
          div ! class_ "post_text" $ toHtml $ postText $ post postA
          div ! class_ "post_details" $ do
            "Posted by "; a ! href (toValue (username $ author postA)) $ (toHtml $ fullName $ author postA)) posts

edit :: FSProfile -> Maybe String -> Html
edit profile mcurrentCity = do
  h2 "Edit My Profile"
  div ! id "profile_pic" $ do
    case profilePicId profile of
      Just profilePId -> img ! src (toValue $ "/profile_pics/" ++ (S.unpack profilePId))
      Nothing -> ""
    br
    a ! href "/profile_pics/new" $ "Upload a new profile picture"
  form ! action (toValue $ "/profiles/" ++ username profile) !
         method "POST" !
         id "edit_profile_form" $ do
    field "first_name" True $ firstName profile
    field "middle_name" False $ middleName profile
    field "last_name" True $ lastName profile
    field "current_city" False $ mcurrentCity
    p $ input ! type_ "submit" ! value "Update Profile"
