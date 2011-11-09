import Profile

import Control.Monad.Trans

import RoutedServer
import qualified Data.ByteString.Char8 as S
import Text.StringTemplate

import System.IO

main :: IO ()
main = do
  runHttpServer 8000 routing

routing = [ routeTop $ routeConst $ resp301 "/home",
                    routeMap apps
                  , routeFileSys mimeMap (dirRedir "/index.html") "public"
                  ]
apps = [("profile", routeVar $ routeFn profileController)]

profileController req = do
  let profileId = (head $ reqPathParams req)
  profile <- lift $ run $ findProfile (read $ S.unpack profileId)
  let template = getTemplate "views/profile.html"
  let view = render $ setAttribute "id" profileId $
          newSTMP template
  return $ mkHtmlResp stat200 $ view
