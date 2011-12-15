{-# LANGUAGE OverloadedStrings #-}

import Data.IterIO.Http
import Data.IterIO.HttpRoute hiding (routeFileSys)
import Data.Monoid

import Functions
import RestController
import RoutedServer

import Controllers.WelcomeController
import Controllers.ProfilePicsController
import Controllers.SearchController
import Controllers.ProfilesController
import Controllers.PostsController
import Controllers.FriendsController
import Extensions.PRMController

import LIO.DCLabel

main :: IO ()
main = do
  runHttpServer 8000 $ routing

routeFileSys = routeGenFileSys defaultFileSystemCalls

routing :: (DCPrivTCB -> HttpRequestHandler DC ())
routing priv = runHttpRoute $ mconcat
                  [ routeTop $ routeConst $ resp301 "/welcome",
                    routeName "welcome" $ routeRestController (WelcomeController),
                    routeName "search" $ routeRestController (SearchController),
                    routeName "posts" $ routeRestController (PostController),
                    routeName "profiles" $ routeRestController (ProfilesController),
                    routeName "profile_pics" $ routeRestController (ProfilePicsController),
                    routeName "friends" $ routeRestController (FriendsController),
                    routeName "pleaserobme" $ routeRestController (PRMController priv),
                    routeFileSys mimeMap (const mempty) "public"
                  ]


