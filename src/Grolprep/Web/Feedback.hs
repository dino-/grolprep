{-# LANGUAGE FlexibleContexts #-}

-- Copyright: 2009, 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Betty Diegel <bdiegel@usa.net>

module Grolprep.Web.Feedback
   where

import Control.Monad.Except ( liftM, runExceptT, throwError )
import Data.Maybe
import Data.List
import Network.CGI hiding (urlEncode)
import Network.HTTP
import Network.Stream
import Network.URI hiding (path)
import System.FilePath
import Text.Printf
import Text.XHtml.Strict 

import Grolprep.Common.Log
import Grolprep.Common.Util ( formattedDate, getVarFilePath, mkdir )
import Grolprep.Web.Session
import Grolprep.Web.Util


{- Feedback handler failures -}
data FeedbackException a
   = ChalRespFail String
   | RecaptchaFail String
   | HttpFail (Response a)
   | NetFail ConnError
   deriving Show


{- Convenience type, not really necessary -}
type FeedbackResult a = Either (FeedbackException a) ()


{- Log feedback handler failures -}
logFeedbackException :: FeedbackException a -> IO ()
logFeedbackException (ChalRespFail s) = llog NOTICE s
logFeedbackException (RecaptchaFail s) = llog NOTICE s
logFeedbackException (HttpFail r) = llog ERROR $ show r
logFeedbackException (NetFail ce) = llog ERROR $ show ce


{- HTML pages and forms 
-}

formFeedback :: String -> String -> String -> String -> App CGIResult
formFeedback msg addr subj comment =  do
   pubkey <- getConfig "recaptcha-site-key"
   burl <- liftIO baseUrl
   fbPage <- liftIO $ page ["css/feedback.css"] $ 
      form ! [ method "POST" 
             , action $ burl ++ "/feedback"
             ] << (
         [ p << msg 
         , p << [ label << "Email: ", widget "text" "email" [ value addr ] ]
         , p << [ label << "Subject: ", widget "text" "subject" [ value subj ] ]
         , p << [ label << "Comment: "
                , (textarea ! [rows "10", cols "40", name "comment"]) << comment 
                ]
         , p << [ label << "", reCaptchaWidget pubkey ]
         , p << [ label << ""
                , submit "feedback" "Submit" ! [theclass "button"] 
                , input ! [ theclass "button"
                          , strAttr "value" "Cancel"
                          , strAttr "type" "button"
                          , strAttr "onClick" ("parent.location='" ++ burl ++ "'") 
                          ] 
                ]
         ] ) 
   output $ renderHtml fbPage


reCaptchaWidget :: String -> Html
reCaptchaWidget k = do 
   script ! 
      [ thetype "text/javascript"
      , src $ "http://api.recaptcha.net/challenge?k=" ++ k 
      ] << noHtml
            

pageThankYou :: String -> Html
pageThankYou burl = 
   p << "Thank you for your feedback!"
   +++
   p << anchor ! [ href $ burl ]
      << "Return to the GROLPrep main page."


pageServerError :: String -> Html
pageServerError burl =
   p << "Server error.  Try again later"
   +++
   p << anchor ! [ href $ burl ]
      << "Return to the GROLPrep main page."


feedbackPage :: App CGIResult
feedbackPage = do 
   llog INFO "feedbackPage"
   formFeedback "" "" "" "" 

{- Handles feedback form submit
-}
feedbackHandler :: App CGIResult
feedbackHandler = do
   llog INFO "feedbackHandler"

   email <- liftM fromJust $ getInput "email"
   subj <- liftM fromJust $ getInput "subject"
   comment <- liftM fromJust $ getInput "comment"
   commentorIp <- remoteAddr
   challenge <- liftM fromJust $ getInput "recaptcha_challenge_field"
   response <- liftM fromJust $ getInput "recaptcha_response_field"
   privkey <- getConfig "recaptcha-secret-key"
      
   e <- liftIO $ verifyChallengeResponse privkey commentorIp challenge response

   burl <- liftIO baseUrl

   case e of 
      Left (ChalRespFail _) -> formFeedback "The CAPTCHA solution was incorrect.  Please try again." email subj comment
      Left err -> do 
         liftIO $ logFeedbackException err
         output $ renderHtml $ pageServerError burl
      Right _ -> do
         thankyouPage <- liftIO $ do
            fname <- formattedDate "%Y%m%d-%H%M%S"
            saveFeedback fname $ printf
               "Submitted: %s\nIP: %s\nEmail: %s\nSubject: %s\nComment: %s\n"
               fname commentorIp email subj comment
            page [] $ pageThankYou burl
         output $ renderHtml thankyouPage
   

{- Verifies the challenge response with the reCaptcha server
-}
verifyChallengeResponse :: String -> String -> String -> String -> IO (FeedbackResult String)
verifyChallengeResponse k ip challenge response =  runExceptT $ do
   let params = urlEncodeVars 
                  [ ("privatekey", k)
                  , ("remoteip", ip)
                  , ("challenge", challenge) 
                  , ("response", response)
                  ]
   let uri = fromJust $ parseURI "http://api-verify.recaptcha.net/verify"
   let hdrs = [ mkHeader HdrHost "api-verify.recaptcha.net"
              , mkHeader HdrContentType "application/x-www-form-urlencoded" 
              , mkHeader HdrContentLength (show $ length params) 
              ]
   let httpReq = ((mkRequest POST uri) :: Request String) { rqBody = params, rqHeaders = hdrs }

   result <- liftIO $ simpleHTTP httpReq
   resp <- either (throwError . NetFail) return result
   respBody <- evalCode resp
   evalBody respBody

   where 
      evalCode r@(Response (x, _, _) _ _ b)
         | x > 2 = throwError $ HttpFail r
         | otherwise = return b

      evalBody b 
         | isInfixOf "true" b = return ()
         | isInfixOf "incorrect-captcha-sol" b = throwError $ ChalRespFail b 
         | otherwise = throwError $ RecaptchaFail b


{- Saves feedback submission to file 
-}
saveFeedback :: String -> String -> IO ()
saveFeedback fname fcontent = do
   let fbDir = getVarFilePath "feedback"
   mkdir fbDir
   let path = fbDir </> fname
   logM DEBUG $ "path: " ++ path ++ " content:" ++ fcontent
   writeFile path fcontent
