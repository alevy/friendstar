{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Data.IterIO
import Data.IterIO.Http
import qualified Data.ListLike as LL

import ProfilesController
import RoutedServer

main :: IO ()
main = do
  runHttpServer 8000 routing

routing = [ routeTop $ routeConst $ resp301 "/home",
                    routeName "profiles" $ routeRestController (ProfilesController),
                    routeFileSys mimeMap (dirRedir "/index.html") "public"
                  ]


