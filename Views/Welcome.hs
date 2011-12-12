{-# LANGUAGE OverloadedStrings #-}
module Views.Welcome where

import Prelude hiding (head, id, div)
import Data.Char
import qualified Data.ByteString.Char8 as S
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (form, label)

field :: String -> Bool -> Html
field _name req = div ! class_ "field" $ do
  label ! for idName $
    if req then do
      toHtml $ ((capitalize _name) ++ ":")
      sup "*"
    else do
      toHtml $ ((capitalize _name) ++ ":")
  input ! type_ "text" ! name (toValue $ "profile[" ++ _name ++ "]") ! id idName
  where capitalize (x:xs) = toUpper x : xs
        capitalize [] = []
        idName = toValue $ ("profile_" ++ _name)

index :: Maybe S.ByteString -> Html
index mcurrentUser = do
  case mcurrentUser of
    Just currentUser -> p $ do
              "Welcome Back! Go to your profile "
              a ! href (toValue $ "/profiles/" ++ (S.unpack $ currentUser)) $ "here"
    Nothing -> do
      form ! action "/profiles" ! method "POST" ! id "new_profile_form" $ do
        field "username" True
        field "first_name" True
        field "middle_name" False
        field "last_name" True
        input ! type_ "submit" ! value "Create Profile"