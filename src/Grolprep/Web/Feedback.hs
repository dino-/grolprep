-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Grolprep.Web.Feedback
   where

import Control.Monad()
import Control.Monad.Error
import Data.Maybe
import Data.List
import Network.CGI hiding (urlEncode)
import Network.HTTP
import Network.HTTP.Base
import Network.HTTP.Headers
import Network.Stream
import Network.URI hiding (path)
import System.FilePath
import Text.Printf
import Text.XHtml.Strict 

import Grolprep.Common.Log
import Grolprep.Common.Util
import Grolprep.Web.Session
import Grolprep.Web.Util
import Paths_grolprep


{- Feedback handler failures -}
data FeedbackException a
   = ChalRespFail String
   | RecaptchaFail String
   | HttpFail (Response a)
   | NetFail ConnError
   deriving Show


{- It's bullshit that we have to do this -}
instance Error (FeedbackException a) where
   noMsg = ChalRespFail "SHOULD NEVER SEE THIS"


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
   pubkey <- getConfig "recaptcha-public-key"
   fbPage <- liftIO $ page ["css/feedback.css"] $ 
      form ! [ method "POST" 
             , action $ baseUrl ++ "/feedback"
             ] << (
         [ p << msg 
         , p << [ label << "Email: ", widget "text" "email" [ value addr ] ]
         , p << [ label << "Subject: ", widget "text" "subject" [ value subj ] ]
         , p << [ label << "Comment: "
                , (textarea ! [rows "10", cols "40", name "comment"]) << comment 
                ]
         , p << [ label << "", reCaptchaWidget pubkey ]
         , p << [ label << "", submit "feedback" "Submit" ! [theclass "button"] ]
         ] ) 
   output $ renderHtml fbPage


reCaptchaWidget :: String -> Html
reCaptchaWidget k = do 
   script ! 
      [ thetype "text/javascript"
      , src $ "http://api.recaptcha.net/challenge?k=" ++ k 
      ] << noHtml
            

pageThankYou :: Html
pageThankYou = 
   p << "Thank you for your feedback!"
   +++
   p << anchor ! [ href $ baseUrl ]
      << "Return to the GROLPrep main page."


pageServerError :: Html
pageServerError =
   p << "Server error.  Try again later"
   +++
   p << anchor ! [ href $ baseUrl ]
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
   privkey <- getConfig "recaptcha-private-key"
      
   e <- liftIO $ verifyChallengeResponse privkey commentorIp challenge response

   case e of 
      Left (ChalRespFail _) -> formFeedback "The CAPTCHA solution was incorrect.  Please try again." email subj comment
      Left err -> do 
         liftIO $ logFeedbackException err
         output $ renderHtml pageServerError
      Right _ -> do
         thankyouPage <- liftIO $ do
            fname <- formattedDate "%Y%m%d-%H%M%S"
            saveFeedback fname $ printf
               "Submitted: %s\nIP: %s\nEmail: %s\nSubject: %s\nComment: %s\n"
               fname commentorIp email subj comment
            page [] $ pageThankYou
         output $ renderHtml thankyouPage
   

{- Verifies the challenge response with the reCaptcha server
-}
verifyChallengeResponse :: String -> String -> String -> String -> IO (FeedbackResult String)
verifyChallengeResponse k ip challenge response =  runErrorT $ do
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
   fbDir <- getDataFileName $ "feedback"
   mkdir fbDir
   let path = fbDir </> fname
   logM DEBUG $ "path: " ++ path ++ " content:" ++ fcontent
   writeFile path fcontent
