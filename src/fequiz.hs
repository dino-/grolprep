-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad
import Data.Bits
import Data.List
import Data.Maybe
import Network.CGI
import System.Log
import Text.Printf
import Text.Regex
import Text.XHtml.Strict

import Fequiz.Data
import Fequiz.Log
import Fequiz.Session
import Paths_fequiz


{- Utility functions and definitions
-}

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)


isRight :: Either a b -> Bool
isRight = not . isLeft


appName, appVersion, appId :: String
appName = "fequiz"
appVersion = "1.0.0.2"
appId = printf "%s-%s" appName appVersion


readSessionCookie :: (MonadCGI m) => m (Maybe Session)
readSessionCookie = readCookie appId


getButtonPressed :: (MonadCGI m) => m (Maybe String)
getButtonPressed = do
   let keys = ["btnStart", "btnPose", "btnAnswer", "btnQuit"]
   mbvs <- mapM getInput keys
   let mbfs = zipWith (\i a -> maybe Nothing (const $ Just a) i)
         mbvs keys
   return $ foldr mplus Nothing mbfs


{- This function allows us to rely on the cabal data-files mechanism 
   to generate relative URLs for HTML pages
-}
getRelDataFileName :: String -> IO String
getRelDataFileName s = do
   fullPath <- getDataFileName s
   return $ (++) "../" $ maybe s head $
      matchRegex (mkRegex "(share.*)$") fullPath


page :: (HTML a) => a -> Html -> IO Html
page t b = do
   cssPath <- getRelDataFileName "css/question.css"
   return (
      (header <<
         thetitle << t
         +++
         thelink noHtml ! [href cssPath, rel "stylesheet", 
             thetype "text/css"]
      )
      +++
      body << ([h, b])
      )

   where h = p << ((printf "%s %s" appName appVersion) :: String)


nextProblem :: Session -> IO (Maybe Problem)
nextProblem (Session stype _ scurr _) = do
   let (Set questionsPath) = stype

   eps <- liftM parseProblems $
      getDataFileName questionsPath >>= readFile
   let ps = either undefined snd eps

   return $ if (scurr < length ps)
      then Just (ps !! scurr)
      else Nothing


{- Forms
-}

formStart :: Html
formStart = form << (
   p << "Please select type of study"
   +++
   p << select ! [name "file", size "12"] <<
      (   option ! [value "questions/element1", selected] << "Element 1 (170 questions)"
      +++ option ! [value "questions/subelement3a"] << "Subelement 3A - Operating procedures (40 questions)"
      +++ option ! [value "questions/subelement3b"] << "Subelement 3B - Radio wave propagation (42 questions)"
      +++ option ! [value "questions/subelement3c"] << "Subelement 3C - Radio practice (69 questions)"
      +++ option ! [value "questions/subelement3d"] << "Subelement 3D - Electrical principles (202 questions)"
      +++ option ! [value "questions/subelement3e"] << "Subelement 3E - Circuit components (150 questions)"
      +++ option ! [value "questions/subelement3f"] << "Subelement 3F - Practical circuits (139 questions)"
      +++ option ! [value "questions/subelement3g"] << "Subelement 3G - Signals and emissions (131 questions)"
      +++ option ! [value "questions/subelement3h"] << "Subelement 3H - Antennas and feedlines (143 questions)"
      +++ option ! [value "questions/element8"] << "Element 8 (321 questions)"
      +++ option ! [value "questions/small1"] << "small1 (6 questions)"
      )
   +++ p << submit "btnStart" "Start study session"
   )


formPoseProblem :: Problem -> Html
formPoseProblem (Problem n q eas) = form << (
   [ paragraph ! [theclass "question"] << ((show n) ++ "] " ++ q)
   , thediv ! [theclass "answer-box"] << (ansControls eas)
   , submit "btnPose" "Proceed" +++ submit "btnQuit" "Cancel test session"
   ] )
   where
      ansControls eas' = map f $ zip [0..] $ map extractAnswer eas'
         where
            f :: (Int, String) -> Html
            f (n', a) = paragraph << ((radio "answer" (show n')) +++ a)


formAnswer :: Bool -> Html
formAnswer correct = form << (
   paragraph << (show correct) +++
   submit "btnAnswer" "Next question" +++
   submit "btnQuit" "Cancel test session"
   )


{- Action handlers
-}

actionInitialize :: CGI CGIResult
actionInitialize = do
   llog INFO "actionInitialize"

   let c = newCookie appId ""
   deleteCookie c

   startPage <- liftIO $ page appName formStart
   output $ renderHtml startPage


actionSetupSession :: CGI CGIResult
actionSetupSession = do
   llog INFO "actionSetupSession"

   questionsPath <- liftM fromJust $ getInput "file"

   let session = Session (Set questionsPath) Nothing 0 0
   let cookie = newCookie appId $ show session
   setCookie cookie

   actionNextProblem session


actionNextProblem :: Session -> CGI CGIResult
actionNextProblem session = do
   llog INFO "actionNextProblem"

   mbnp <- liftIO $ nextProblem session
   maybe actionInitialize posePageResult mbnp

   where
      posePageResult np = do
         posePage <- liftIO $ page appName $ formPoseProblem np
         output $ renderHtml $ posePage


actionCorrectProblem :: CGI CGIResult
actionCorrectProblem = do
   llog INFO "actionCorrectProblem"

   -- Here we'll add results to the state and setCookie

   -- Get current session and extract some things from it
   session@(Session _ _ curr ansList)
      <- liftM fromJust readSessionCookie

   -- Evaluate the user's answer
   mbnp <- liftIO $ nextProblem session
   let problem@(Problem _ _ as) = fromJust mbnp

   answer <- liftM fromJust $ readInput "answer"

   let correct = isRight $ as !! answer
   let newAnsList = case correct of
         True  -> setBit ansList curr
         False -> clearBit ansList curr

   -- Make the new session and set it
   let newSession =
         session { sessCurr = curr + 1 , sessResults = newAnsList }
   let cookie = newCookie appId $ show newSession
   setCookie cookie

   answerPage <- liftIO $ page appName $ formAnswer correct
   output $ renderHtml answerPage


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
      (_,            Just "btnPose"  ) -> actionCorrectProblem
      (_,            Just "btnQuit"  ) -> actionInitialize
      (Just session, _               ) -> actionNextProblem session
      (_,            _               ) -> actionInitialize


main :: IO ()
main = do
   initLogging "/var/tmp/fequiz.log" DEBUG
   runCGI $ handleErrors cgiMain
