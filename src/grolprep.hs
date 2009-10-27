-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad
import Data.Map ( lookup )
import Data.Maybe
import Network.CGI
import Prelude hiding ( lookup )

import Grolprep.Common.Conf
import Grolprep.Common.Log
import Grolprep.Web.Feedback
import Grolprep.Web.Session
import Grolprep.Web.Study

import Paths_grolprep

{- This is sort of our main event entry point. The web application
   starts here and all form submits come through here.
-}
cgiMain :: App CGIResult
cgiMain = do
   path <- pathInfo
   llog DEBUG $ "pathInfo: " ++ path

   case (path) of
      "/feedback" -> dispatchFeedback
      "/study"    -> dispatchStudy
      _           -> dispatchStudy


main :: IO ()
main = do
   confMap <- liftM parseToMap $ 
      getDataFileName "grolprep.conf" >>= readFile
   let logPath = fromJust $ lookup "log-path" confMap
   let logPriority = read . fromJust $ lookup "log-priority" confMap

   initLogging logPath logPriority

   runApp confMap cgiMain
