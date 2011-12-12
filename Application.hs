{-# LANGUAGE OverloadedStrings #-}
module Application where

import qualified Data.ByteString.Char8 as S

import Text.Blaze
import Text.Blaze.Renderer.Utf8
import LIO.DCLabel
import LIO.LIO (liftLIO)

import qualified Views.Layout as Layout
import RestController
import Profile

currentUser :: RestControllerContainer t DC (Maybe FSProfile)
currentUser = do
  uname <- usernameFromSession
  case uname of
    Just _username' -> do
      liftLIO $ runM $ findProfileByUsername _username'
    Nothing -> return Nothing

renderTemplate :: Html -> RestControllerContainer t DC ()
renderTemplate tmpl = do
  cu <- currentUser
  render "text/html" $ renderHtml $ Layout.application cu tmpl

instance ToHtml a => ToHtml (Maybe a) where
  toHtml (Just a) = toHtml a
  toHtml Nothing = toHtml ("" :: String)

instance ToValue a => ToValue (Maybe a) where
  toValue (Just a) = toValue a
  toValue Nothing = toValue ("" :: String)
