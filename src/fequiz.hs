-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Data.List
import Data.Maybe
import Network.CGI
import System.Log

import Fequiz.Common.Log
import Fequiz.Common.Util
import Fequiz.Web.Session
import Fequiz.Web.Study


{- This is sort of our main event entry point. The web application
   starts here and all form submits come through here.
-}
cgiMain :: App CGIResult
cgiMain = do
   qs <- queryString
   llog DEBUG $ "query string: " ++ qs

   mbSession <- getSession
   llog DEBUG $ "session: " ++ show mbSession

   -- Figure out which form button was used for submit
   mbForm <- getButtonPressed
   llog DEBUG $ "form button: " ++ show mbForm

   -- Map session status and form button pressed into actions
   case (mbSession, mbForm) of
      (Nothing, Nothing       ) -> actionInitialize
      (Nothing, Just ActStart ) -> actionSetupSession
      (_,       Just ActPose  ) -> actionCorrectProblem
      (_,       Just ActQuit  ) -> actionInitialize
      (Just _,  _             ) -> actionNextProblem
      (_,       _             ) -> actionInitialize


main :: IO ()
main = do
   initLogging "/var/tmp/fequiz.log" DEBUG
   runApp cgiMain
