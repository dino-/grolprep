-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Data.List
import Network.CGI
import System.Log

import Fequiz.Common.Log
import Fequiz.Web.Feedback
import Fequiz.Web.Session
import Fequiz.Web.Study


{- This is sort of our main event entry point. The web application
   starts here and all form submits come through here.
-}
cgiMain :: App CGIResult
cgiMain = do
   qs <- queryString
   llog DEBUG $ "query string: " ++ qs

   path <- pathInfo
   llog DEBUG $ "pathInfo: " ++ path

   case (path) of
      "/feedback" -> dispatchFeedback
      _           -> dispatchStudy


main :: IO ()
main = do
   initLogging "/var/tmp/fequiz.log" DEBUG
   runApp cgiMain
