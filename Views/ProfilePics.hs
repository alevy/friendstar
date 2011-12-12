{-# LANGUAGE OverloadedStrings #-}
module Views.ProfilePics where

import Prelude hiding (head, id, div)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (form)

new :: Html
new = do
  h2 "Upload Profile Picture"
  form ! action "/profile_pics/" !
         method "POST" !
         id "new_profile_pic_form" !
         enctype "multipart/form-data" $ do
    p $ do
      input ! type_ "file" ! name "profile_pic[image]" ! id "profile_pic_image"
      input ! type_ "submit" ! value "Upload"