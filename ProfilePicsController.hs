{-# LANGUAGE OverloadedStrings #-}
module ProfilePicsController where

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

import Application
import RestController
import Blob
import Profile

data ProfilePicsController = ProfilePicsController

imageFromParams :: Params -> (L.ByteString, S.ByteString)
imageFromParams params = (imageData, contentType)
  where (Just (imageData, headers)) = lookup "profile_pic[image]" params
        (Just contentType) = lookup "content-type" headers

instance RestController ProfilePicsController where

  restShow _ imageId _ = do
    let image = run $ getBlob imageId
    render (blobMimeType image) (L.pack $ S.unpack $ blobData image)

  restNew _ _ = do
    context <- contextFromMUsername `fmap` usernameFromSession
    renderTemplate "views/profile_pics/new.html" $ context

  restCreate _ params = do
    (Just user) <- usernameFromSession
    let (imageData, contentType) = imageFromParams params
    let image = run $ saveBlob $ FSBlob {
      blobId = Nothing,
      blobData = S.pack $ L.unpack $ imageData,
      blobMimeType = S.unpack $ contentType
    }
    let currentProfile = (run $ findProfileByUsername user) {
      profilePicId = blobId image
    }
    return $ run $ saveProfile currentProfile
    redirectTo ("/profiles/edit/" ++ (S.unpack user))