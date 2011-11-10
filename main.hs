{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
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
    profile <- lift $ run $ findProfile (read $ S.unpack profileId)
    let template = getTemplate "views/profile.html"
    let view = render $ setAttribute "profile" profile $
          newSTMP template
    return $ mkHtmlResp stat200 $ view

  restEdit self req = do
    let profileId = (head $ reqPathParams req)
    profile <- lift $ run $ findProfile (read $ S.unpack profileId)
    let template = getTemplate "views/edit.html"
    let view = render $ setAttribute "profile" profile $
          newSTMP template
    return $ mkHtmlResp stat200 $ view

  restUpdate self req = do
    lift $ putStrLn "Update request\n"
    -- XXX: Doing something wrong with the monads
    --lift $ putStrLn (show $ withParm "profile.firstName" req)
    let profileId = (head $ reqPathParams req)
    profile <- lift $ run $ findProfile (read $ S.unpack profileId)
    let template = getTemplate "views/profile.html"
    let view = render $ setAttribute "profile" profile $
          newSTMP template
    return $ mkHtmlResp stat200 $ view

type L = L.ByteString

withParm :: (MonadIO m) => String -> HttpReq ()
         -> Iter L m a -> Iter L m (Maybe a)
withParm pName req iter = foldForm req handlePart Nothing
  where handlePart result part = if ffName part == S.pack pName
				    then Just <$> iter
				    else nullI >> return result

