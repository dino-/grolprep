-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad
import Data.List.Split
import Data.Map ( lookup )
import Data.Maybe
import Network.CGI
import Prelude hiding ( lookup )
import Text.Printf

import Grolprep.Common.Conf
import Grolprep.Common.Log
import Grolprep.Web.Feedback
import Grolprep.Web.Session
import Grolprep.Web.Study

import Paths_grolprep


{- This is our main event entry point. All requests come through here.
-}
cgiMain :: App CGIResult
cgiMain = do
   haveSession <- liftM isJust getSession
   method <- requestMethod
   path <- pathInfo

   llog DEBUG $ printf "haveSession: %s" $ show haveSession
   llog DEBUG $ printf "method: %s  path: %s" method path

   dispatch haveSession method $ filter (/= "") $ splitOn "/" path


dispatch :: Bool -> String -> [String] -> App CGIResult
dispatch _     "GET"  ["feedback"]         = feedbackPage
dispatch _     "POST" ["feedback"]         = feedbackHandler
dispatch _     "GET"  ["study", "setup"]   = formSetup
dispatch _     "POST" ["study", "setup"]   = setupSession
dispatch _     "GET"  ["init"]             = initialize
dispatch False _      _                    = initialize
dispatch _     "POST" ["study", "problem"] = evalProblem
dispatch _     _      ["study", "next"]    = prepareForNext
dispatch True  _      _                    = formProblem


main :: IO ()
main = do
   confMap <- liftM parseToMap $ 
      getDataFileName "grolprep.conf" >>= readFile
   let logPath = fromJust $ lookup "log-path" confMap
   let logPriority = read . fromJust $ lookup "log-priority" confMap

   initLogging logPath logPriority

   runApp confMap cgiMain
