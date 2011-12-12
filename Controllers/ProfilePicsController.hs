{-# LANGUAGE OverloadedStrings #-}
module Controllers.ProfilePicsController where

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import LIO.LIO (liftLIO)

import Application
import RestController
import Blob
import Profile

import Views.ProfilePics

data ProfilePicsController = ProfilePicsController

imageFromParams :: Params -> (L.ByteString, S.ByteString)
imageFromParams params = (imageData, contentType)
  where (Just (imageData, headers)) = lookup "profile_pic[image]" params
        (Just contentType) = lookup "content-type" headers

instance RestController ProfilePicsController where

  restShow _ imageId _ = do
    image <- liftLIO $ run $ getBlob imageId
    render (blobMimeType image) (L.pack $ S.unpack $ blobData image)

  restNew _ _ = do
    renderTemplate new

  restCreate _ params = do
    (Just user) <- usernameFromSession
    let (imageData, contentType) = imageFromParams params
    image <- liftLIO $ run $ saveBlob $ FSBlob {
      blobId = Nothing,
      blobData = S.pack $ L.unpack $ imageData,
      blobMimeType = S.unpack $ contentType
    }
    currentProfile <- do
      c <- liftLIO $ run $ findProfileByUsername user
      return $ c {
        profilePicId = blobId image
      }
    _ <- return $ run $ saveProfile currentProfile
    redirectTo ("/profiles/edit/" ++ (S.unpack user))