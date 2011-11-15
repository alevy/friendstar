{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module RoutedServer (mkHttpServer,
                     runHttpServer,
                     mimeMap,
                     getTemplate,
                     paramMap,
                     module Data.IterIO.Http,
                     module Data.IterIO.HttpRoute) where

import Prelude hiding (catch, div)
import Control.Applicative ((<$>))
import Control.Monad
import Control.Concurrent
import Control.Exception
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import Data.Map
import Data.Monoid
import qualified Network.Socket as Net
import qualified OpenSSL.Session as SSL
import System.IO
import System.IO.Unsafe
import System.Posix
import Text.Regex

import Data.IterIO
import Data.IterIO.Iter
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
      return (handleI h, enumHandle h)-- `inumFinally` liftIO (hClose h))
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

simpleServer :: MonadIO m => Iter L.ByteString m ()  -- Output to web browser
            -> Onum L.ByteString m ()  -- Input from web browser
            -> [HttpRoute m ()]
            -> m ()
simpleServer iter enum routes = enum |$ inumHttpServer server .| iter
   where server = ioHttpServer $ runHttpRoute $ mconcat routes

mimeMap :: String -> S.ByteString
mimeMap = unsafePerformIO $ do
     path <- findMimeTypes ["conf/mime.types"
                           , "/etc/mime.types"
                           , "/var/www/conf/mime.types"]
     enumFile path |$ mimeTypesI "application/octet-stream"
     where
       findMimeTypes (h:t) = do exist <- fileExist h
                                if exist then return h else findMimeTypes t
       findMimeTypes []    = return "mime.types" -- cause error

--- Tentative
sanitizePath :: FilePath -> FilePath
sanitizePath path = path -- TODO!!

getTemplate :: FilePath -> String
getTemplate path = unsafePerformIO $ do
    file <- openFile (sanitizePath path) ReadMode
    hGetContents file

paramMap :: MonadIO m => String -> HttpReq s -> Iter L.ByteString m (Map String L.ByteString)
paramMap objName req = foldForm req handlePart empty
  where handlePart accm field = do
          val <- pureI
          return $ maybe (accm) (\x -> insert (head x) val accm) (matchRegex rg $ S.unpack $ ffName field)
        rg = mkRegex $ objName ++ "\\[([^]]+)\\]"

