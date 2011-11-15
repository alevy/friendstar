{-# LANGUAGE OverloadedStrings #-}

module RestController ( RestController,
                        restIndex,
                        restShow,
                        restEdit,
                        restNew,
                        restCreate,
                        restUpdate,
                        restDestroy,
                        routeRestController,
                        setSession,
                        destroySession,
                        usernameFromSession,
                        render,
                        renderTemplate,
                        redirectTo,
                        getHttpReq,
                        respond404) where

import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Monoid
import Data.IterIO
import Data.IterIO.Iter
import Data.IterIO.Http
import Data.IterIO.HttpRoute
import Text.Hastache

import System.IO.Unsafe

type L = L.ByteString
type S = S.ByteString

newtype RestControllerContainer t m a = RestControllerContainer
                                          { runRest :: HttpReq t
                                                      -> HttpResp m
                                                      -> (a, HttpReq t, HttpResp m) }

instance Monad m => Monad (RestControllerContainer t m) where
  return x = RestControllerContainer $ \req resp -> (x, req, resp)

  controller >>= next = RestControllerContainer $ \req resp-> 
                                     let (result, req', resp') = runRest controller req resp
                                     in seq result $ runRest (next result) req' resp'

instance MonadIO m => MonadIO (RestControllerContainer t m) where
  liftIO x = RestControllerContainer $ \req resp -> (result, req, resp)
    where result = unsafePerformIO x

routeRestController :: MonadIO m => RestController a => a -> HttpRoute m s
routeRestController controller = mconcat routes
  where routes = [routeTop $ routeMethod "GET" $ routeFn (_restNoVar controller restIndex),
                  routeMethod "GET" $ routeName "new" $ routeFn (_restNoVar controller restNew),
                  routeMethod "GET" $ routeName "edit" $ routeVar $ routeFn (_restWithVar controller restEdit),
                  routeMethod "POST" $ routeName "destroy" $ routeFn (_restWithVar controller restDestroy),
                  routeMethod "GET" $ routeVar $ routeFn (_restWithVar controller restShow),
                  routeTop $ routeMethod "POST" $ routeFn (_restNoVar controller restCreate),
                  routeMethod "POST" $ routeVar $ routeFn (_restWithVar controller restUpdate)]

respond404 :: Monad m => RestControllerContainer t m ()
respond404 = RestControllerContainer $ \req _ -> ((), req, resp404 req)

redirectTo :: Monad m => String -> RestControllerContainer t m ()
redirectTo path = RestControllerContainer $ \req _ -> ((), req, resp303 path)

render :: Monad m => String -> L -> RestControllerContainer t m ()
render ctype text = RestControllerContainer $ \req resp -> ((), req, mkResp resp)
  where len = S.pack $ "Content-Length: " ++ show (L.length text)
        ctypeHeader = S.pack $ "Content-Type: " ++ ctype
        mkResp resp = resp { respHeaders = respHeaders resp ++ [ctypeHeader, len],
                        respBody = inumPure text }

renderTemplate :: (MonadIO m, Monad m) => FilePath -> MuContext IO -> RestControllerContainer t m ()
renderTemplate tmpl context = do
  str <- liftIO $ hastacheFile defaultConfig tmpl context
  render "text/html" str

setSession :: String -> RestControllerContainer t m ()
setSession cookie = RestControllerContainer $ \req resp ->
  let cookieHeader = S.pack $ "Set-Cookie: _sess=" ++ cookie ++ "; path=/;"
  in ((), req, resp { respHeaders = cookieHeader:(respHeaders resp)})

destroySession :: RestControllerContainer t m ()
destroySession = RestControllerContainer $ \req resp ->
  let cookieHeader = S.pack $ "Set-Cookie: _sess=; path=/; expires=Thu, Jan 01 1970 00:00:00 UTC;"
  in ((), req, resp { respHeaders = cookieHeader:(respHeaders resp)})

usernameFromSession :: RestControllerContainer t m (Maybe S)
usernameFromSession = RestControllerContainer $ \req resp -> (_getUser req, req, resp)
  where _getUser req = foldl (\accm (k, v) -> if k == "_sess" then Just v else accm) Nothing $ reqCookies req

getHttpReq :: RestControllerContainer t m (HttpReq t)
getHttpReq = RestControllerContainer $ \req resp -> (req, req, resp)

class RestController a where
  restIndex :: (MonadIO m, Monad m) => a -> [(S, L)] -> RestControllerContainer t m ()
  restIndex _ _ = respond404

  restShow :: (MonadIO m, Monad m) => a -> S.ByteString -> [(S, L)] -> RestControllerContainer t m ()
  restShow _ _ _ = respond404

  restEdit :: (MonadIO m, Monad m) => a -> S.ByteString -> [(S, L)] -> RestControllerContainer t m ()
  restEdit _ _ _ = respond404

  restNew :: (MonadIO m, Monad m) => a -> [(S, L)] -> RestControllerContainer t m ()
  restNew _ _ = respond404

  restCreate :: (MonadIO m, Monad m) => a -> [(S, L)] -> RestControllerContainer t m ()
  restCreate _ _ = respond404

  restUpdate :: (MonadIO m, Monad m) => a -> S.ByteString -> [(S, L)] -> RestControllerContainer t m ()
  restUpdate _ _ _ = respond404

  restDestroy :: (MonadIO m, Monad m) => a -> S.ByteString -> [(S, L)] -> RestControllerContainer t m ()
  restDestroy _ _ _ = respond404

  _restNoVar :: (MonadIO t, Monad m) => a
                -> (a -> [(S, L)] -> RestControllerContainer s m ())
                -> HttpReq s
                -> Iter L t (HttpResp m)
  _restNoVar self handler req = do
    params <- paramList req
    let (_, _, response) = runRest (handler self params) req $ mkHttpHead $ stat200
    liftIO $ putStrLn $ show response
    return $ response

  _restWithVar :: (MonadIO t, Monad m) => a
                 -> (a -> S.ByteString -> [(S, L)] -> RestControllerContainer s m ())
                 -> HttpReq s
                 -> Iter L t (HttpResp m)
  _restWithVar self handler req = do
    params <- paramList req
    let arg = head $ reqPathParams req
    let (_, _, response) = runRest (handler self arg params) req $ mkHttpHead $ stat200
    liftIO $ putStrLn $ show response
    return $ response

paramList :: (MonadIO t) => HttpReq s -> Iter L t [(S, L)]
paramList req = foldForm req handle []
  where handle accm field = do
          val <- pureI
          return $ (ffName field, val):accm
