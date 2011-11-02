import RoutedServer
import qualified Data.ByteString.Lazy.Char8 as L
import Text.StringTemplate


main :: IO ()
main = do
  runHttpServer 8000 routing

routing = [ routeTop $ routeConst $ resp301 "/home",
                    routeMap apps
                  , routeFileSys mimeMap (dirRedir "/index.html") "public"
                  ]
apps = [("home", routeFn app1)]

--app1 :: (Monad m) => HttpReq s -> Iter L.ByteString m (HttpResp m)
app1 req = do
  let t = newSTMP "Hello $name$\r\n"
  return $ mkHtmlResp stat200 $ L.pack $ render $ setAttribute "name" (reqPath req) t
