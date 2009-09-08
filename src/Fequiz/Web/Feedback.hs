-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Fequiz.Web.Feedback
   where

import Network.CGI
import System.Log
import Text.XHtml.Strict

import Fequiz.Common.Log
import Fequiz.Common.Util
import Fequiz.Web.Session


{- HTML pages and forms 
-}
formFeedback :: Html
formFeedback =  do
   let labelStyle = "display: block; width: 150px; float: left; margin: 2px 4px 6px 4px; text-align: right; vertical-align: top"
   form << ( 
      [ p << [label ! [thestyle labelStyle] << "Email: ", textfield "email"]
      , p << [label ! [thestyle labelStyle] << "Subject: ", textfield "subject"]
      , p << [label ! [thestyle labelStyle] << "Comment: ", (textarea ! [rows "20", cols "40"]) noHtml]
      , p << [label ! [thestyle labelStyle] << "", submit "ActFeedback" "Submit" ! [theclass "button"] ]
      ] ) 
   

{- Action handlers
-}

actionFeedbackPage :: App CGIResult
actionFeedbackPage = do 
   llog INFO "actionFeedbackPage"
   feedbackPage <- liftIO $ page $ formFeedback
   output $ renderHtml feedbackPage

actionFeedbackHandler :: App CGIResult
actionFeedbackHandler = do
   llog INFO "actionFeedbackHandler"
   output $ renderHtml $ form << "Thank you!"
