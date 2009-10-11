-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Grolprep.Web.Feedback
   where

import Control.Monad
import Data.Maybe
import Network.CGI
import System.FilePath
import Text.Printf
import Text.XHtml.Strict

import Grolprep.Common.Log
import Grolprep.Common.Util
import Grolprep.Web.Session
import Grolprep.Web.Util
import Paths_grolprep


{- Dispatches HTML requests for /feedback URLs
-}
dispatchFeedback :: App CGIResult
dispatchFeedback = do
   -- Figure out which form button was used for submit
   mbForm <- getButtonPressed
   llog DEBUG $ "form button: " ++ show mbForm

   -- Map form button pressed into actions
   case (mbForm) of
      (Just ActFeedback ) -> actionFeedbackHandler
      (_                ) -> actionFeedbackPage


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


pageThankYou :: Html
pageThankYou = 
   p << "Thank you for your feedback!"
   +++
   p << anchor ! [ href $ baseUrl ]
      << "Return to the GROLPrep main page."


{- Action handlers
-}

actionFeedbackPage :: App CGIResult
actionFeedbackPage = do 
   llog INFO "actionFeedbackPage"
   feedbackPage <- liftIO $ page [] $ formFeedback
   output $ renderHtml feedbackPage


{- Handles feedback form submit
-}
actionFeedbackHandler :: App CGIResult
actionFeedbackHandler = do
   llog INFO "actionFeedbackHandler"
   email <- liftM fromJust $ getInput "email"
   subj <- liftM fromJust $ getInput "subject"
   comment <- liftM fromJust $ getInput "comment"
   --llog DEBUG $ "email: " ++ email ++ " subj: " ++ subj ++ " comment: " ++ comment
   commentorIp <- remoteAddr
   liftIO $ do       
      fname <- formattedDate "%Y%m%d-%H%M%S"
      saveFeedback fname $ printf
         "Submitted: %s\nIP: %s\nEmail: %s\nSubject: %s\nComment: %s\n"
         fname commentorIp email subj comment
   thankyouPage <- liftIO $ page [] $ pageThankYou
   output $ renderHtml thankyouPage


{- Saves feedback submission to file 
-}
saveFeedback :: String -> String -> IO ()
saveFeedback fname fcontent = do
   fbDir <- getDataFileName $ "feedback"
   mkdir fbDir
   let path = fbDir </> fname
   logM DEBUG $ "path: " ++ path ++ " content:" ++ fcontent
   writeFile path fcontent
