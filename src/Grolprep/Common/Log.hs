-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Grolprep.Common.Log
   ( initLogging, logM, llog, logTest
   , Priority (..)  -- Re-exported from System.Log
   )
   where

import Control.Monad.Trans
import System.Log.Handler.Simple ( fileHandler )
import qualified System.Log.Logger as L ( logM )
import           System.Log.Logger hiding ( logM )
import Text.Printf

import Grolprep.Common.Util


{- Log a message to the root logger with timestamp and priority
   included
-}
logM :: Priority -> String -> IO ()
logM pri msg = do
   fd <- formattedDate "%Y-%m-%d %H:%M:%S %Z"
   let fullMsg = printf "%s %9s> %s" fd (show pri) msg
   L.logM rootLoggerName pri fullMsg


{- Convenience function for logging from within transformered
   computations
-}
llog :: (MonadIO m) => Priority -> String -> m ()
llog p m = liftIO $ logM p m


{- Default hslogger behavior sends logging to stderr. In this case,
   we want it to go to stdout.
-}
initLogging :: String -> Priority -> IO ()
initLogging logPath level = do
   logHandler <- fileHandler logPath level
   updateGlobalLogger rootLoggerName (setHandlers [logHandler])
   updateGlobalLogger rootLoggerName (setLevel level)


{- Test function to generate every kind of log message
-}
logTest :: IO ()
logTest = do
   logM DEBUG     "log test message 1 of 8"
   logM INFO      "log test message 2 of 8"
   logM NOTICE    "log test message 3 of 8"
   logM WARNING   "log test message 4 of 8"
   logM ERROR     "log test message 5 of 8"
   logM CRITICAL  "log test message 6 of 8"
   logM ALERT     "log test message 7 of 8"
   logM EMERGENCY "log test message 8 of 8"
