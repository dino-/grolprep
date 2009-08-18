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


appName, version :: String
appName = "fequiz"
version = "1.0.0.1"


startForm :: Html
startForm = form << (
   p << "Please select type of study"
   +++
   p << select ! [name "file", size "10"] <<
      (   option ! [value "0"] << "Element 1"
      +++ option ! [value "1"] << "Subelement 3A -- Operating procedures"
      +++ option ! [value "2"] << "Subelement 3B -- Radio wave propagation"
      )
   +++ p << submit "start" "Start study session"
   )


problemForm :: Problem -> Html
problemForm (Problem n q eas) = form << (
   [ paragraph << ((show n) ++ "] " ++ q)
   ] ++ (ansControls eas) ++
   [ submit "problem" "Proceed"
   ] )
   where
      ansControls eas' = map f $ zip [0..] $ map extractAnswer eas'
         where
            f :: (Int, String) -> Html
            f (n', a) = paragraph << ((radio "answer" (show n')) +++ a)


page :: (HTML a) => a -> Html -> Html
page t b = header << thetitle << t +++ body << ([h, b])
   where h = p << ((printf "%s %s" appName version) :: String)


startAction = do
   llog DEBUG "executing startAction now"
   output $ renderHtml $ page appName $ startForm


problemAction problem = do
   llog DEBUG "executing problemAction now"
   output $ renderHtml $ page appName $ problemForm problem


cgiMain problem = do
   qs <- queryString
   llog DEBUG $ "query string: " ++ qs

   mbis <- mapM getInput ["problem", "start"]
   let mbas = zipWith (\i a -> maybe Nothing (const $ Just a) i)
         mbis [startAction, (problemAction problem)]

   fromJust $ foldr mplus (Just startAction) mbas


main :: IO ()
main = do
   initLogging "/var/tmp/fequiz.log" DEBUG

   eps <- liftM parseProblems $ readFile "resources/1.txt"

   --either print (\(_,ps) -> print $ head ps) eps
   --either print (\(_,ps) -> print $ take 2 ps) eps

   let problem = either undefined (\(_,ps) -> head ps) eps
   runCGI $ handleErrors $ cgiMain problem
