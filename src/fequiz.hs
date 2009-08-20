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


appName, version, appId :: String
appName = "fequiz"
version = "1.0.0.1"
appId = printf "%s-%s" appName version


startForm :: Html
startForm = form << (
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


problemForm :: Problem -> Html
problemForm (Problem n q eas) = form << (
   [ paragraph << ((show n) ++ "] " ++ q)
   ] ++ (ansControls eas) ++
   --[ submit "problem" "Proceed"
   [ submit "problem" "Proceed" +++ submit "quit" "Cancel test session"
   ] )
   where
      ansControls eas' = map f $ zip [0..] $ map extractAnswer eas'
         where
            f :: (Int, String) -> Html
            f (n', a) = paragraph << ((radio "answer" (show n')) +++ a)


page :: (HTML a) => a -> Html -> Html
page t b = header << thetitle << t +++ body << ([h, b])
   where h = p << ((printf "%s %s" appName version) :: String)


initializeAction = do
   llog DEBUG "initializeAction"

   let c = newCookie appId ""
   deleteCookie c
   output $ renderHtml $ page appName $ startForm


nextProblem :: Session -> IO Problem
nextProblem session = do
   let (Set questionsPath) = sessType session
   let nextProblemIx = length (sessResults session)

   eps <- liftM parseProblems $ readFile questionsPath
   let ps = either undefined snd eps

   return $ ps !! nextProblemIx


setupSessionAction = do
   llog DEBUG "setupSessionAction"

   questionsPath <- liftM fromJust $ getInput "file"

   let session = Session (Set questionsPath) Nothing []
   let cookie = newCookie appId $ show session
   setCookie cookie

   np <- liftIO $ nextProblem session
   problemAction np


problemAction problem = do
   llog DEBUG "problemAction"

   output $ renderHtml $ page appName $ problemForm problem


readSessionCookie :: (MonadCGI m) => m (Maybe Session)
readSessionCookie = readCookie appId


getFormName :: (MonadCGI m) => m (Maybe String)
getFormName = do
   let keys = ["problem", "start", "quit"]
   mbvs <- mapM getInput keys
   let mbfs = zipWith (\i a -> maybe Nothing (const $ Just a) i)
         mbvs keys
   return $ foldr mplus Nothing mbfs


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
      (Nothing, Nothing) -> initializeAction
      (Nothing, Just "start") -> setupSessionAction
      (_, Just "quit") -> initializeAction
      --(Just _, Just "problem") -> problemAction
      (_, _) -> initializeAction


main :: IO ()
main = do
   initLogging "/var/tmp/fequiz.log" DEBUG
   runCGI $ handleErrors cgiMain

   --eps <- liftM parseProblems $ readFile "resources/1.txt"

   --either print (\(_,ps) -> print $ head ps) eps
   --either print (\(_,ps) -> print $ take 2 ps) eps

   --let problem = either undefined (\(_,ps) -> head ps) eps
   --runCGI $ handleErrors $ cgiMain problem
