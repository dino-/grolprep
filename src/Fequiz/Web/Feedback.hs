-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Fequiz.Web.Feedback
   where

import Control.Monad
import Data.Maybe
import Network.CGI
import System.FilePath
import System.Log
import Text.XHtml.Strict

import Fequiz.Common.Log
import Fequiz.Common.Util
import Fequiz.Web.Session
import Fequiz.Web.Util
import Paths_fequiz


{- HTML pages and forms 
-}
formFeedback :: Html
formFeedback =  do
   let labelStyle = "display: block; width: 150px; float: left; margin: 2px 4px 6px 4px; text-align: right; vertical-align: top"
   form ! [ method "POST" ] << ( 
      [ p << [label ! [thestyle labelStyle] << "Email: ", textfield "email"]
      , p << [label ! [thestyle labelStyle] << "Subject: ", textfield "subject"]
      , p << [label ! [thestyle labelStyle] << "Comment: ", 
            (textarea ! [rows "20", cols "40", name "comment"]) noHtml]
      , p << [label ! [thestyle labelStyle] << "", submit "ActFeedback" "Submit" ! [theclass "button"] ]
      ] ) 
   

{- Action handlers
-}

actionFeedbackPage :: App CGIResult
actionFeedbackPage = do 
   llog INFO "actionFeedbackPage"
   feedbackPage <- liftIO $ page $ formFeedback
   output $ renderHtml feedbackPage


{- Handles feedback form submit
-}
actionFeedbackHandler :: App CGIResult
actionFeedbackHandler = do
   llog INFO "actionFeedbackHandler"
   email <- liftM fromJust $ getInput "email"
   subj <- liftM fromJust $ getInput "subject"
   comment <- liftM fromJust $ getInput "comment"
   llog DEBUG $ "email: " ++ email ++ " subj: " ++ subj ++ " comment: " ++ comment
   liftIO $ do       
      fname <- formattedDate "%Y%m%d%H%M%S"
      saveFeedback fname ( "Email:" ++ email ++ "\nSubject:" ++ subj ++ "\nComment:" ++ comment )
   output $ renderHtml $ form << ( "Thank you " ++ email )

{- Saves feedback submission to file 
-}
saveFeedback :: String -> String -> IO ()
saveFeedback fname fcontent = do
   fbDir <- getDataFileName $ "feedback"
   mkdir fbDir
   let path = fbDir </> fname
   logM DEBUG $ "path: " ++ path ++ " content:" ++ fcontent
   writeFile path fcontent
