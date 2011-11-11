{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Map
import Text.Regex
import Text.StringTemplate
import Text.StringTemplate.GenericStandard

import System.IO
import Data.IterIO
import Data.IterIO.Http
import qualified Data.ListLike as LL

import Profile
import RoutedServer

main :: IO ()
main = do
  runHttpServer 8000 routing

routing = [ routeTop $ routeConst $ resp301 "/home",
                    routeName "profiles" $ routeRestController (ProfilesController 1),
                    routeFileSys mimeMap (dirRedir "/index.html") "public"
                  ]

data ProfilesController = ProfilesController Integer

instance RestController ProfilesController where
  restIndex self req = do
    let template = getTemplate "views/profiles/index.html"
    let view = render $ newSTMP template
    return $ mkHtmlResp stat200 $ view

  restShow self req = do
    let profileId = (head $ reqPathParams req)
    profile <- liftIO $ run $ findProfile (read $ S.unpack profileId)
    let template = getTemplate "views/profile.html"
    let view = render $ setAttribute "profile" profile $
          newSTMP template
    return $ mkHtmlResp stat200 $ view

  restEdit self req = do
    let profileId = (head $ reqPathParams req)
    profile <- liftIO $ run $ findProfile (read $ S.unpack profileId)
    let template = getTemplate "views/edit.html"
    let view = render $ setAttribute "profile" profile $
          newSTMP template
    return $ mkHtmlResp stat200 $ view

  restUpdate self req = do
    p <- paramMap "profile" req
    let profileId = head $ reqPathParams req
    let profileId = read $ S.unpack profileId
    let profile = (profileFromMap p) { profileId = Just profileId }
--    liftIO $ run $ saveProfile profile
    liftIO $ putStrLn $ (show profile)
    let template = getTemplate "views/profile.html"
    let view = render $ setAttribute "profile" profile $
          newSTMP template
    return $ mkHtmlResp stat200 $ view

type L = L.ByteString

profileFromMap :: Map String L -> FSProfile
profileFromMap map = defaultFSProfile {
  firstName = L.unpack $ map ! "first_name",
  lastName = L.unpack $ map ! "last_name",
  currentCity = fmap L.unpack $ "current_city" `Data.Map.lookup` map
}

paramMap :: MonadIO m => String -> HttpReq s -> Iter L m (Map String L)
paramMap objName req = foldForm req handlePart empty
  where handlePart accm field = do
          val <- pureI
          return $ maybe (accm) (\x -> insert (head x) val accm) (matchRegex rg $ S.unpack $ ffName field)
        rg = mkRegex $ objName ++ "\\[([^]]+)\\]"

