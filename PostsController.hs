module PostsController where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import Text.StringTemplate
import Text.StringTemplate.GenericStandard

import RoutedServer
import Profile

data PostController = PostController

instance RestController PostController where
  restCreate self req = return $ resp404 req

