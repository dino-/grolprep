-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Fequiz.Common
   where

import Control.Monad
import Data.List
import Data.Maybe
import Network.CGI
import Text.Printf
import Text.Regex
import Text.XHtml.Strict

import Fequiz.Session
import Paths_fequiz


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


{- Convenience function to deal with some of the repetitive parts
   included in every HTML document
-}
page :: Html -> IO Html
page b = do
   cssPath <- getRelDataFileName "css/question.css"
   return (
      (header <<
         thetitle << appId
         +++
         thelink noHtml ! [href cssPath, rel "stylesheet", 
             thetype "text/css"]
      )
      +++
      body << ([(p << appId), b])
      )
