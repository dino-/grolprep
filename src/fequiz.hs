-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Data.List
import Data.Maybe
import Network.CGI
import System.Log

import Fequiz.Common
import Fequiz.Log
import Fequiz.Session
import Fequiz.Study


{- This is sort of our main event entry point. The web application
   starts here and all form submits come through here.
-}
cgiMain :: App CGIResult
cgiMain = do
   qs <- queryString
   llog DEBUG $ "query string: " ++ qs

   -- Extract the cookie
   mbCookie <- readSessionCookie
   llog DEBUG $ "cookie: " ++ show mbCookie

   -- Figure out which form button was used for submit
   mbForm <- getButtonPressed
   llog DEBUG $ "form button: " ++ show mbForm

   -- Map cookie status and form button pressed into actions
   case (mbCookie, mbForm) of
      (Nothing,      Nothing       ) -> actionInitialize
      (Nothing,      Just ActStart ) -> actionSetupSession
      (_,            Just ActPose  ) -> actionCorrectProblem
      (_,            Just ActQuit  ) -> actionInitialize
      (Just session, _             ) -> actionNextProblem session
      (_,            _             ) -> actionInitialize


main :: IO ()
main = do
   initLogging "/var/tmp/fequiz.log" DEBUG
   runApp cgiMain
