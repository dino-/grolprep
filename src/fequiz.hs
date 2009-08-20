-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad
import Data.List
import Data.Maybe
import Network.CGI
import System.Log
import Text.Printf
import Text.XHtml.Strict

import Fequiz.Data
import Fequiz.Log
import Fequiz.Session


{- Utility functions and definitions
-}

appName, version, appId :: String
appName = "fequiz"
version = "1.0.0.1"
appId = printf "%s-%s" appName version


readSessionCookie :: (MonadCGI m) => m (Maybe Session)
readSessionCookie = readCookie appId


getFormName :: (MonadCGI m) => m (Maybe String)
getFormName = do
   let keys = ["start", "pose", "answer", "quit"]
   mbvs <- mapM getInput keys
   let mbfs = zipWith (\i a -> maybe Nothing (const $ Just a) i)
         mbvs keys
   return $ foldr mplus Nothing mbfs


page :: (HTML a) => a -> Html -> Html
page t b = header << thetitle << t +++ body << ([h, b])
   where h = p << ((printf "%s %s" appName version) :: String)


nextProblem :: Session -> IO Problem
nextProblem session = do
   let (Set questionsPath) = sessType session
   let nextProblemIx = length (sessResults session)

   eps <- liftM parseProblems $ readFile questionsPath
   let ps = either undefined snd eps

   return $ ps !! nextProblemIx


{- Forms
-}

formStart :: Html
formStart = form << (
   p << "Please select type of study"
   +++
   p << select ! [name "file", size "10"] <<
      (   option ! [value "resources/1.txt"] << "Element 1"
      +++ option ! [value "resources/3a.txt"] << "Subelement 3A -- Operating procedures"
      +++ option ! [value "resources/3b.txt"] << "Subelement 3B -- Radio wave propagation"
      +++ option ! [value "util/resources/small1.txt"] << "small1"
      )
   +++ p << submit "start" "Start study session"
   )


formPoseProblem :: Problem -> Html
formPoseProblem (Problem n q eas) = form << (
   [ paragraph << ((show n) ++ "] " ++ q)
   ] ++ (ansControls eas) ++
   [ submit "pose" "Proceed" +++ submit "quit" "Cancel test session"
   ] )
   where
      ansControls eas' = map f $ zip [0..] $ map extractAnswer eas'
         where
            f :: (Int, String) -> Html
            f (n', a) = paragraph << ((radio "answer" (show n')) +++ a)


{- Action handlers
-}

actionInitialize = do
   llog DEBUG "actionInitialize"

   let c = newCookie appId ""
   deleteCookie c
   output $ renderHtml $ page appName $ formStart


actionSetupSession = do
   llog DEBUG "actionSetupSession"

   questionsPath <- liftM fromJust $ getInput "file"

   let session = Session (Set questionsPath) Nothing []
   let cookie = newCookie appId $ show session
   setCookie cookie

   actionNextProblem session


actionNextProblem session = do
   np <- liftIO $ nextProblem session
   output $ renderHtml $ page appName $ formPoseProblem np


{-
actionCorrectProblem = do
   llog DEBUG "problemAction"

   -- Here we'll add results to the state and setCookie

   --output $ renderHtml $ page appName $ formPoseProblem problem
-}


{- main program
-}

cgiMain :: CGI CGIResult
cgiMain = do
   qs <- queryString
   llog DEBUG $ "query string: " ++ qs

   -- Extract the cookie
   mbCookie <- readSessionCookie
   llog DEBUG $ "cookie: " ++ show mbCookie

   -- Figure out which form button was used for submit
   mbForm <- getFormName
   llog DEBUG $ "form button: " ++ show mbForm

   case (mbCookie, mbForm) of
      (Nothing, Nothing) -> actionInitialize
      (Nothing, Just "start") -> actionSetupSession
      --(_, Just "pose") -> actionCorrectProblem
      (Just session, Just "pose") -> actionNextProblem session
      --(_, Just "answer") -> actionNextProblem
      (_, Just "quit") -> actionInitialize
      (_, _) -> actionInitialize


main :: IO ()
main = do
   initLogging "/var/tmp/fequiz.log" DEBUG
   runCGI $ handleErrors cgiMain
