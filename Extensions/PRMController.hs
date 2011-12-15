{-# LANGUAGE OverloadedStrings #-}

module Extensions.PRMController where

import Prelude hiding (id, div)
import qualified Prelude (id)
import Data.URLEncoded hiding (lookup)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (label, form)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import LIO.DCLabel (DCPrivTCB)
import LIO.LIO hiding (label)

import LIO.Http

import Application
import Profile
import RestController

newtype PRMController = PRMController DCPrivTCB
  deriving Show

instance RestController PRMController where
  restIndex _ _ = renderTemplate $ do
    h2 "Please Rob Me!"
    h3 "- steal sh*1"
    form ! action "/pleaserobme/" ! method "POST" $ do
      div ! class_ "field" $ do
        label ! for "user" $ "Who's house should we rob?"
        br
        input ! type_ "text" ! name "user" ! id "user" ! placeholder "username"
      p $ input ! type_ "submit" ! value "Commence robbery"

  restCreate self params = do
    let (Just user) = fmap (S.pack . L.unpack . fst) $ lookup "user" params
    profile <- liftLIO $ run $ findProfileByUsername user
    let (PRMController privilege) = self
    city <- liftLIO $ unlabelP privilege $ currentCity profile
    let cityStr = maybe "none" (urlShow . ((%=) ("q" :: String))) city
    liftLIO $ simpleLHTTP (getRequest ("http://localhost:8080/" ++ cityStr))
    renderTemplate $ do
      h2 $ toHtml $ maybe "Not checked in" show city
      p $ "I posted that sh*1 on the webs!"
