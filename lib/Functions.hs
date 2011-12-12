module Functions where

import Conversions

import Data.IterIO
import Data.IterIO.HttpRoute
import qualified Data.Time.Clock as Clock
import LIO.TCB
import LIO.LIO (liftLIO)
import LIO.DCLabel
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types

getCurrentTime :: DC Clock.UTCTime
getCurrentTime = ioTCB $ Clock.getCurrentTime

defaultFileSystemCalls :: FileSystemCalls Fd DC
defaultFileSystemCalls = FileSystemCalls { fs_stat = liftLIO . ioTCB . getFileStatus
                                         , fs_open = liftLIO . ioTCB . pathToFd
                                         , fs_close = liftLIO . ioTCB . closeFd
                                         , fs_fstat = liftLIO . ioTCB . getFdStatus
                                         , fs_enum = liftLIO . ioTCB . fdToOnum
                                         }
    where pathToFd path = openFd path ReadOnly Nothing defaultFileFlags
          fdToOnum fd = do h <- fdToHandle fd
                           return $ onumIOtoOnumDC $ enumHandle h
