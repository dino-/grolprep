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

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)


isRight :: Either a b -> Bool
isRight = not . isLeft


appName, version, appId :: String
appName = "fequiz"
version = "1.0.0.1"
appId = printf "%s-%s" appName version


readSessionCookie :: (MonadCGI m) => m (Maybe Session)
readSessionCookie = readCookie appId


getButtonPressed :: (MonadCGI m) => m (Maybe String)
getButtonPressed = do
   let keys = ["btnStart", "btnPose", "btnAnswer", "btnQuit"]
   mbvs <- mapM getInput keys
   let mbfs = zipWith (\i a -> maybe Nothing (const $ Just a) i)
         mbvs keys
   return $ foldr mplus Nothing mbfs


page :: (HTML a) => a -> Html -> Html
page t b = header << thetitle << t +++ body << ([h, b])
   where h = p << ((printf "%s %s" appName version) :: String)


{- Do we really need this?

currProblem :: Session -> IO Problem
currProblem session = do
   let (Set questionsPath) = sessType session
   let currProblemIx = length (sessResults session) - 1

   eps <- liftM parseProblems $ readFile questionsPath
   let ps = either undefined snd eps

   return $ ps !! currProblemIx
-}


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
   +++ p << submit "btnStart" "Start study session"
   )


formPoseProblem :: Problem -> Html
formPoseProblem (Problem n q eas) = form << (
   [ paragraph << ((show n) ++ "] " ++ q)
   ] ++ (ansControls eas) ++
   [ submit "btnPose" "Proceed" +++ submit "btnQuit" "Cancel test session"
   ] )
   where
      ansControls eas' = map f $ zip [0..] $ map extractAnswer eas'
         where
            f :: (Int, String) -> Html
            f (n', a) = paragraph << ((radio "answer" (show n')) +++ a)


formAnswer :: Bool -> Html
formAnswer correct = form << (
   paragraph << (show correct) +++
   submit "btnAnswer" "Next question" +++ submit "btnQuit" "Cancel test session"
   )


{- Action handlers
-}

actionInitialize :: CGI CGIResult
actionInitialize = do
   llog INFO "actionInitialize"

   let c = newCookie appId ""
   deleteCookie c
   output $ renderHtml $ page appName $ formStart


actionSetupSession :: CGI CGIResult
actionSetupSession = do
   llog INFO "actionSetupSession"

   questionsPath <- liftM fromJust $ getInput "file"

   let session = Session (Set questionsPath) Nothing []
   let cookie = newCookie appId $ show session
   setCookie cookie

   actionNextProblem session


actionNextProblem :: Session -> CGI CGIResult
actionNextProblem session = do
   llog INFO "actionNextProblem"

   np <- liftIO $ nextProblem session
   output $ renderHtml $ page appName $ formPoseProblem np


actionCorrectProblem :: CGI CGIResult
actionCorrectProblem = do
   llog INFO "actionCorrectProblem"

   -- Here we'll add results to the state and setCookie

   -- Get current session and extract some things from it
   session <- liftM fromJust readSessionCookie
   let ansList = sessResults session

   -- Evaluate the user's answer
   problem@(Problem _ _ as) <- liftIO $ nextProblem session
   answer <- liftM fromJust $ readInput "answer"
   let correct = isRight $ as !! answer

   -- Make the new session and set it
   let newSession = session { sessResults = correct : ansList }
   let cookie = newCookie appId $ show newSession
   setCookie cookie

   output $ renderHtml $ page appName $ formAnswer correct


{- main program
-}

{- This is sort of our main event entry point. The web application
   starts here and all form submits come through here.
-}
cgiMain :: CGI CGIResult
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
      (Nothing,      Nothing         ) -> actionInitialize
      (Nothing,      Just "btnStart" ) -> actionSetupSession
      (Just session, Just "btnAnswer") -> actionNextProblem session
      (_,            Just "btnPose"  ) -> actionCorrectProblem
      (_,            Just "btnQuit"  ) -> actionInitialize
      (_,            _               ) -> actionInitialize


main :: IO ()
main = do
   initLogging "/var/tmp/fequiz.log" DEBUG
   runCGI $ handleErrors cgiMain
