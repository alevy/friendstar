{-# LANGUAGE OverloadedStrings #-}

module RoutedServer (
  runHttpServer,
  mimeMap,
  paramMap
) where

import Conversions

import Control.Concurrent
import Control.Monad
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Map

import qualified Network.Socket as Net
import System.IO
import System.Posix
import Text.Regex

import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.HttpRoute

import qualified LIO.TCB as LIO
import qualified LIO.LIO as LIO
import LIO.DCLabel
import DCLabel.TCB

import System.IO.Unsafe


type L = L.ByteString

myListen :: Net.PortNumber -> IO Net.Socket
myListen pn = do
  sock <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
  Net.setSocketOption sock Net.ReuseAddr 1
  Net.bindSocket sock (Net.SockAddrInet pn Net.iNADDR_ANY)
  Net.listen sock Net.maxListenQueue
  return sock

httpAccept :: Net.Socket -> IO (Iter L IO (), Onum L IO a)
httpAccept sock = do
  (s, _) <- Net.accept sock
  (iter, enum) <- mkInsecure s
  return (iter, enum)
  where
    mkInsecure s = do
      h <- Net.socketToHandle s ReadWriteMode
      hSetBuffering h NoBuffering
      return (handleI h, enumHandle h)

runHttpServer :: Net.PortNumber
              -> (DCPrivTCB -> HttpRequestHandler DC ())
              -> IO ()
runHttpServer port lrh = do
  sock <- myListen port
  forever $ do
    (iter, enum) <- httpAccept sock
    _ <- forkIO $ simpleServer iter enum lrh
    return ()

simpleServer :: Iter L IO ()
              -> Onum L IO L
              -> (DCPrivTCB -> HttpRequestHandler DC ())
              -> IO ()
simpleServer iter enum lrh = do
  let dcEnum = onumIOtoOnumDC enum
  let dcIter = iterIOtoIterDC iter
  (result, _) <- evalDC $ dcEnum |$ secureInumHttpServer lrh .| dcIter
  return result

userFromAuthCode :: Maybe S.ByteString -> Maybe String
userFromAuthCode mAuthCode = fmap extractUser mAuthCode
  where extractUser b64u = S.unpack $ S.takeWhile (/= ':') $ decodeLenient b64u

replace :: (a -> Bool) -> a -> [a] -> [a]
replace f a (x:xs) | f x = a:(replace f a xs)
                 | otherwise = x:(replace f a xs)
replace _ _ [] = []

secureInumHttpServer :: (DCPrivTCB -> HttpRequestHandler DC ()) -> Inum L L DC ()
secureInumHttpServer lrh = mkInumM $
  do
    req <- httpReqI
    case userFromAuthCode (fmap (S.drop 6) $ Prelude.lookup "authorization" (reqHeaders req)) of
      Just user -> do
        let l = newDC user (<>)
        LIO.liftLIO $ LIO.lowerClr l
        let pathPrefix = takeWhile (/= '/') $ dropWhile (== '/') $ S.unpack $ reqPath req
        let privilege = createPrivTCB $ newPriv pathPrefix
        let resultReq = req {
          reqHeaders = replace ((== "authorization") . fst) ("authorization", S.pack user) $
                        reqHeaders req
        }
        resp <- liftI $ inumHttpBody req .| lrh privilege resultReq
        resultLabel <- LIO.liftLIO $ LIO.getLabel
        if resultLabel `LIO.leq` l then
          irun $ enumHttpResp resp Nothing
        else
          irun $ enumHttpResp (mkHttpHead stat500) Nothing
      Nothing -> do
        let authRequired = mkHttpHead stat401
        irun $ enumHttpResp (authRequired
          { respHeaders = "WWW-Authenticate: Basic realm=\"Hails\"":(respHeaders authRequired)})
          Nothing

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

paramMap :: [(S.ByteString, (L.ByteString, [(S.ByteString, S.ByteString)]))] -> String -> Map String L.ByteString
paramMap prms objName = foldl handle empty prms
  where handle accm (k, v) = do
          maybe (accm) (\x -> insert (head x) (fst $ v) accm) (matchRegex rg $ S.unpack k)
        rg = mkRegex $ objName ++ "\\[([^]]+)\\]"
