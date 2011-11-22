{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Data.IterIO.Http

import RestController
import RoutedServer

import WelcomeController
import ProfilePicsController
import SearchController
import SessionsController
import ProfilesController
import PostsController
import FriendsController

main :: IO ()
main = do
  runHttpServer 8000 routing

routing :: [HttpRoute IO s]
routing = [ routeTop $ routeConst $ resp301 "/welcome",
                    routeName "welcome" $ routeRestController (WelcomeController),
                    routeName "search" $ routeRestController (SearchController),
                    routeName "sessions" $ routeRestController (SessionsController),
                    routeName "posts" $ routeRestController (PostController),
                    routeName "profiles" $ routeRestController (ProfilesController),
                    routeName "profile_pics" $ routeRestController (ProfilePicsController),
                    routeName "friends" $ routeRestController (FriendsController),
                    routeFileSys mimeMap (dirRedir "/index.html") "public"
                  ]


