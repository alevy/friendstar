{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module RoutedServer (mkHttpServer,
                     runHttpServer,
                     mimeMap,
                     module Data.IterIO.Http,
                     module Data.IterIO.HttpRoute) where

import Prelude hiding (catch, head, id, div)
import Data.Monoid
import Control.Monad
import Control.Concurrent
import Control.Exception
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Network.Socket as Net
import qualified OpenSSL.Session as SSL
import System.IO
import System.IO.Unsafe
import System.Posix

import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.HttpRoute
import Data.IterIO.SSL

type L = L.ByteString

data HttpServer = HttpServer {
      hsListenSock :: !Net.Socket
    , hsSslCtx :: !(Maybe SSL.SSLContext)
    }

myListen :: Net.PortNumber -> IO Net.Socket
myListen pn = do
  sock <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
  Net.setSocketOption sock Net.ReuseAddr 1
  Net.bindSocket sock (Net.SockAddrInet pn Net.iNADDR_ANY)
  Net.listen sock Net.maxListenQueue
  return sock

httpAccept :: HttpServer -> IO (Net.SockAddr, Iter L IO (), Onum L IO a)
httpAccept hs = do
  (s, addr) <- Net.accept $ hsListenSock hs
  (iter, enum) <- maybe (mkInsecure s) (mkSecure s) (hsSslCtx hs)
  return (addr, iter, enum)
  where
    mkInsecure s = do
      h <- Net.socketToHandle s ReadWriteMode
      hSetBuffering h NoBuffering
      return (handleI h, enumHandle h `inumFinally` liftIO (hClose h))
    mkSecure s ctx = iterSSL ctx s True `catch` \e@(SomeException _) -> do
                       hPutStrLn stderr (show e)
                       Net.sClose s
                       return (nullI, inumNull)

mkHttpServer :: Net.PortNumber -> Maybe SSL.SSLContext -> IO HttpServer
mkHttpServer port mctx = do
  sock <- myListen port
  return $ HttpServer { hsListenSock = sock
                      , hsSslCtx = mctx
                      }

runHttpServer :: Net.PortNumber
              -> [HttpRoute IO ()]
              -> IO ()
runHttpServer port routing = do
  server <- mkHttpServer port Nothing
  forever $ do
    (addr, iter, enum) <- httpAccept server
    _ <- forkIO $ simpleServer iter enum routing
    return ()

simpleServer :: Iter L.ByteString IO ()  -- Output to web browser
            -> Onum L.ByteString IO ()  -- Input from web browser
            -> [HttpRoute IO ()]
            -> IO ()
simpleServer iter enum routes = enum |$ inumHttpServer server .| iter
   where server = ioHttpServer $ runHttpRoute $ mconcat routes

mimeMap :: String -> S8.ByteString
mimeMap = unsafePerformIO $ do
     path <- findMimeTypes ["conf/mime.types"
                           , "/etc/mime.types"
                           , "/var/www/conf/mime.types"]
     enumFile path |$ mimeTypesI "application/octet-stream"
     where
       findMimeTypes (h:t) = do exist <- fileExist h
                                if exist then return h else findMimeTypes t
       findMimeTypes []    = return "mime.types" -- cause error
