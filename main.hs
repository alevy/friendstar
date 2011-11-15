{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Data.IterIO
import Data.IterIO.Http
import qualified Data.ListLike as LL

import RestController
import RoutedServer

import SessionsController
import ProfilesController
import PostsController

main :: IO ()
main = do
  runHttpServer 8000 routing

routing = [ routeTop $ routeConst $ resp301 "/index.html",
                    routeName "sessions" $ routeRestController (SessionsController),
                    routeName "posts" $ routeRestController (PostController),
                    routeName "profiles" $ routeRestController (ProfilesController),
                    routeFileSys mimeMap (dirRedir "/index.html") "public"
                  ]


