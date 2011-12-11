{-# LANGUAGE OverloadedStrings #-}

module RestController ( RestController,
                        restIndex,
                        restShow,
                        restEdit,
                        restNew,
                        restCreate,
                        restUpdate,
                        restDestroy,
                        Params,
                        routeRestController,
                        setSession,
                        destroySession,
                        usernameFromSession,
                        render,
                        addVar,
                        addGeneric,
                        addGenericList,
                        emptyContext,
                        renderTemplate,
                        redirectTo,
                        getHttpReq,
                        respond404) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Data
import Data.Monoid
import Data.IterIO
import Data.IterIO.Iter
import Data.IterIO.Http
import Data.IterIO.HttpRoute
import LIO.DCLabel
import Text.Hastache
import Text.Hastache.Context

import System.IO.Unsafe

type L = L.ByteString
type S = S.ByteString

type RestControllerContainer t m a = StateT (HttpReq t, HttpResp m) m a

--newtype RestControllerContainer t m a = RestControllerContainer
--                                          { runRest :: HttpReq t
--                                                      -> HttpResp m
--                                                      -> (a, HttpReq t, HttpResp m) }

routeRestController :: RestController a => a -> HttpRoute DC s
routeRestController controller = mconcat routes
  where routes = [routeTop $ routeMethod "GET" $ routeFn (_restNoVar controller restIndex),
                  routeMethod "GET" $ routeName "new" $ routeFn (_restNoVar controller restNew),
                  routeMethod "GET" $ routeName "edit" $ routeVar $ routeFn (_restWithVar controller restEdit),
                  routeMethod "POST" $ routeName "destroy" $ routeVar $ routeFn (_restWithVar controller restDestroy),
                  routeMethod "GET" $ routeVar $ routeFn (_restWithVar controller restShow),
                  routeTop $ routeMethod "POST" $ routeFn (_restNoVar controller restCreate),
                  routeMethod "POST" $ routeVar $ routeFn (_restWithVar controller restUpdate)]

respond404 :: RestControllerContainer t DC ()
respond404 = StateT $ \(req, _) -> return $ ((), (req, resp404 req))

redirectTo :: String -> RestControllerContainer t DC ()
redirectTo path = StateT $ \(req, _) -> return $ ((), (req, resp303 path))

render :: String -> L -> RestControllerContainer t DC ()
render ctype text = StateT $ \(req, resp) -> return $ ((), (req, mkResp resp))
  where len = S.pack $ "Content-Length: " ++ show (L.length text)
        ctypeHeader = S.pack $ "Content-Type: " ++ ctype
        mkResp resp = resp { respHeaders = respHeaders resp ++ [ctypeHeader, len],
                        respBody = inumPure text }

-- | Returns an empty context for Hastache templates. Pass this to renderTemplate if
-- the template does not depend on any variables, or as an initial building block for
-- 'addVar' and 'addGeneric'
emptyContext :: MuContext IO
emptyContext _ = MuBool False

-- | Adds the key-value pair (name, var) to context.
-- For example,
--  In controller:
--    renderTemplae "mytemplate.html" $ addVar "num_posts" 4 $ addVar "name" "Frank" $ emptyContext
--  Then in the view:
--    {{ name }} has {{ num_posts }} posts.
--  Will render to:
--    Frank has 4 posts.
addVar :: (MuVar a) => S -> a -> MuContext IO -> MuContext IO
addVar name var context = check
  where check str | str == name = MuVariable var
                  | (S.unpack str) == ((S.unpack name) ++ "?") = MuBool True
                  | True = context str

-- | Adds the key-value pair (name, var) to context, where var is
-- an instance of 'Data.Data'.
addGeneric :: (Data a) => S -> a -> MuContext IO -> MuContext IO
addGeneric name var context = check
  where check x | x == name = MuBool True
                | (prefix x) == name = varCtx (postfix x)
                | otherwise = context x
        prefix x = S.pack $ takeWhile (/= '.') $ S.unpack x
        postfix x = S.pack $ tail $ dropWhile (/= '.') $ S.unpack x
        varCtx = mkGenericContext var

addGenericList :: (Data a) => S -> [a] -> MuContext IO -> MuContext IO
addGenericList name list context = (\x -> if x == name then MuList flist else context x)
  where flist = fmap mkGenericContext list

renderTemplate :: FilePath -> MuContext IO -> RestControllerContainer t DC ()
renderTemplate tmpl context = do
  str <- liftIO $ hastacheFile (defaultConfig { muTemplateFileDir = Just "views" }) tmpl context
  render "text/html" str

setSession :: String -> RestControllerContainer t DC ()
setSession cookie = StateT $ \(req, resp) ->
  let cookieHeader = S.pack $ "Set-Cookie: _sess=" ++ cookie ++ "; path=/;"
  in return $ ((), (req, resp { respHeaders = cookieHeader:(respHeaders resp)}))

destroySession :: RestControllerContainer t DC ()
destroySession = StateT $ \(req, resp) ->
  let cookieHeader = S.pack $ "Set-Cookie: _sess=; path=/; expires=Thu, Jan 01 1970 00:00:00 UTC;"
  in return $ ((), (req, resp { respHeaders = cookieHeader:(respHeaders resp)}))

usernameFromSession :: RestControllerContainer t DC (Maybe S)
usernameFromSession = StateT $ \(req, resp) -> (_getUser req, (req, resp))
  where _getUser req = foldl (\accm (k, v) -> if k == "_sess" then Just v else accm) Nothing $ reqCookies req

getHttpReq :: RestControllerContainer t DC (HttpReq t)
getHttpReq = StateT $ \(req, resp) -> return $ (req, (req, resp))

type Params = [(S, (L, [(S,S)]))]

class RestController a where
  restIndex :: a -> Params -> RestControllerContainer t DC ()
  restIndex _ _ = respond404

  restShow :: a -> S.ByteString -> Params -> RestControllerContainer t DC ()
  restShow _ _ _ = respond404

  restEdit :: a -> S.ByteString -> Params -> RestControllerContainer t DC ()
  restEdit _ _ _ = respond404

  restNew :: a -> Params -> RestControllerContainer t DC ()
  restNew _ _ = respond404

  restCreate :: a -> Params -> RestControllerContainer t DC ()
  restCreate _ _ = respond404

  restUpdate :: a -> S.ByteString -> Params -> RestControllerContainer t DC ()
  restUpdate _ _ _ = respond404

  restDestroy :: a -> S.ByteString -> Params -> RestControllerContainer t DC ()
  restDestroy _ _ _ = respond404

  _restNoVar :: a
                -> (a -> Params -> RestControllerContainer s DC ())
                -> HttpReq s
                -> Iter L DC (HttpResp DC)
  _restNoVar self handler req = do
    params <- paramList req
    (_, _, response) <- runStateT (handler self params) (req, mkHttpHead stat200)
    return $ response

  _restWithVar :: a
                 -> (a -> S.ByteString -> Params -> RestControllerContainer s DC ())
                 -> HttpReq s
                 -> Iter L DC (HttpResp DC)
  _restWithVar self handler req = do
    params <- paramList req
    let arg = head $ reqPathParams req
    let (_, _, response) = runStateT (handler self arg params) (req, mkHttpHead stat200)
    return $ response

paramList :: HttpReq s -> Iter L DC Params
paramList req = foldForm req handle []
  where handle accm field = do
          val <- pureI
          return $ (ffName field, (val, ffHeaders field)):accm
