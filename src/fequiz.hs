-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad
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

{- Remove an indexed element from a list
-}
removeFromList :: Int -> [a] -> [a]
removeFromList i xs = take i xs ++ drop (i + 1) xs


{- Some identifying info for this application
-}
appName, appVersion, appId :: String
appName = "fequiz"
appVersion = "1.0.0.3"
appId = printf "%s-%s" appName appVersion


{- Convenience wrapper for reading the specific named cookie for this 
   application
-}
readSessionCookie :: (MonadCGI m) => m (Maybe Session)
readSessionCookie = readCookie appId


{- Convenience wrapper for setting a new session cookie
-}
setSessionCookie :: (MonadCGI m) => Session -> m ()
setSessionCookie session = do
   let cookie = newCookie appId $ show session
   setCookie cookie


{- When a specific form submit button is pressed, and it has a name 
   attribute, that attribute becomes a key in the request with the 
   value attribute as the value. This can be used to determine if a 
   button was pressed at all to generate the request and if so which 
   one. 

   But it's bad to rely on the hard-coded button's display text as it 
   may change.

   This function checks for the existance of a specific set of buttons 
   and if it's there returns the key, not the value.
-}
getButtonPressed :: (MonadCGI m) => m (Maybe String)
getButtonPressed = do
   let keys = ["btnStart", "btnPose", "btnAnswer", "btnQuit"]
   mbvs <- mapM getInput keys
   let mbfs = zipWith (\i a -> maybe Nothing (const $ Just a) i)
         mbvs keys
   return $ foldr mplus Nothing mbfs


{- This function allows us to rely on the cabal data-files mechanism 
   to generate relative URLs for HTML pages.

   It's a bit of a hack, but this returns the part of the path starting 
   with the share directory and continuing to the end.
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
nextProblem (Session stype _ _ _ scurr slist) = do
   let (Set questionsPath) = stype

   eps <- liftM parseProblems $
      getDataFileName questionsPath >>= readFile
   let ps = either undefined snd eps

   return $ if (scurr < length slist)
      then Just (ps !! (slist !! scurr))
      else Nothing


{- HTML pages and forms 
-}

formStart :: Html
formStart = form <<
   fieldset << (
   legend << "Please select type of study"
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
   +++ p << submit "btnStart" "Start study session" ! [theclass "button"]
   )


headingStats :: Session -> Html
headingStats (Session _ pass passCurr passTot _ list) =
   p << (printf "Pass %d, question %d of %d total in this pass"
      pass passCurr passTot :: String)
   +++
   p << (printf "%d (%0.1f%%) correct so far for this pass"
      correct perc :: String)

   where
      correct = passTot - (length list)

      perc :: Float
      perc = (fromIntegral correct / fromIntegral passTot) * 100


formPoseProblem :: Problem -> Html
formPoseProblem (Problem n q eas) = formCancel +++ form << (
   [ paragraph ! [theclass "question"] << ((show n) ++ "] " ++ q)
   , thediv << (ansControls eas)
   , submit "btnPose" "Proceed" ! [theclass "button"]
   ] )
   where
      ansControls eas' = map f $ zip [0..] $ map extractAnswer eas'
         where
            f :: (Int, String) -> Html
            f (n', a) =
               p << ((radio "answer" (show n') ! [theclass "hanging"])
                    +++ label << a)


formAnswer :: Int -> Problem -> Html
formAnswer g (Problem n q eas) = formCancel +++ form << (
   [ correctness (eas !! g)
   , paragraph ! [theclass "question"] << ((show n) ++ "] " ++ q)
   , thediv << (ansLines eas)
   , submit "btnAnswer" "Next question" ! [theclass "button"]
   ] )
   where
      correctness (Right _) = p ! [theclass "correct-ans"] << "CORRECT"
      correctness _         =
         p ! [theclass "incorrect-ans"] << "INCORRECT"

      ansLines eas' = map f $ zip [0..] eas'
         where
            f :: (Int, Either String String) -> Html
            f (n', Left  a)
               | n' == g    = p ! [theclass "incorrect-ans"] << a
               | otherwise  = p << a
            f (_ , Right a) = p ! [theclass "correct-ans"] << a


formCancel :: Html
formCancel = form << (
   submit "btnQuit" "Cancel test session" ! [theclass "button"]
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

   numQuestions <- liftIO $ do
      eps <- liftM parseProblems $
         getDataFileName questionsPath >>= readFile
      let ps = either undefined snd eps
      return $ length ps

   let questionNumbers = [0..(numQuestions - 1)]
   let session = Session (Set questionsPath) 1 0
         (length questionNumbers) 0 questionNumbers
   setSessionCookie session

   actionNextProblem session


actionNextProblem :: Session -> CGI CGIResult
actionNextProblem session@(Session _ pass passCurr _ _ list) = do
   llog INFO "actionNextProblem"

   mbnp <- liftIO $ nextProblem session

   case (mbnp, null list) of
      (Just np, _    ) -> do
         let newSession =
               session { sessPassCurr = passCurr + 1 }
         setSessionCookie newSession
         posePageResult newSession np
      (_      , True ) -> actionInitialize
      (_      , False) -> do
         let newSession =
               session
               { sessPass = pass + 1
               , sessPassCurr = 0
               , sessPassTot = (length list)
               , sessCurr = 0
               }
         setSessionCookie newSession

         actionNextProblem newSession

   where
      posePageResult ns np = do
         posePage <- liftIO $ page appName $ formPoseProblem np
         output $ renderHtml $ ((headingStats ns) +++ posePage)


actionCorrectProblem :: CGI CGIResult
actionCorrectProblem = do
   llog INFO "actionCorrectProblem"

   -- Get current session and extract some things from it
   session@(Session _ _ _ _ curr list)
      <- liftM fromJust readSessionCookie

   -- Evaluate the user's answer
   mbnp <- liftIO $ nextProblem session
   let problem@(Problem _ _ as) = fromJust mbnp

   answer <- liftM fromJust $ readInput "answer"

   let (newCurr, newList) = case (as !! answer) of
         Right _ -> (curr, removeFromList curr list)
         Left _  -> (curr + 1, list)

   -- Make the new session and set it
   let newSession =
         session { sessCurr = newCurr , sessList = newList }
   setSessionCookie newSession

   answerPage <- liftIO $ page appName $ formAnswer answer problem
   output $ renderHtml ((headingStats newSession) +++ answerPage)


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
