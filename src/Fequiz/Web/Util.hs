-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Fequiz.Web.Util
   where

import Control.Monad
import Data.List
import Data.Maybe
import Network.CGI
import Text.XHtml.Strict

import Fequiz.Common.Util


{- This represents a type-safe enumeration of the possible form actions
   that are responded to by this application

   Serialized values of this type are the submit button identifiers,
   all of this achieved with read and show for safety.
-}
data Action
   = ActStart
   | ActPose
   | ActAnswer
   | ActFeedback
   | ActQuit
   deriving (Read, Show)


{- The base URL of the application
-}
baseUrl :: String
baseUrl = "/fequiz/bin/fequiz.cgi"


{- If you add a constructor the the Action type above, you MUST add
   it to the list of constructed datum below.
-}
allActions :: [Action]
allActions = [ActStart, ActPose, ActAnswer, ActFeedback, ActQuit]


{- When a specific form submit button is pressed, and it has a name 
   attribute, that attribute becomes a key in the request with the 
   value attribute as the value. This can be used to determine if a 
   button was pressed at all to generate the request and if so which 
   one. 

   But it's bad to rely on the hard-coded button's display text as it 
   may change.

   This function checks for the existance of a specific set of buttons 
   and if it's there returns the key, not the value.

   This key, in our application, is also an instance of the Action
   type.
-}
getButtonPressed :: (MonadCGI m) => m (Maybe Action)
getButtonPressed = do
   mbvs <- mapM getInput $ map show allActions
   let mbfs = zipWith (\i a -> maybe Nothing (const $ Just a) i)
         mbvs allActions
   return $ foldr mplus Nothing mbfs


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
      body << ([(p << appId), b]  ++ footer )
      )

footer :: [Html]
footer =  
--   [ anchor ! [href "/fequiz/bin/fequiz.cgi/about"] << "About" 
--   , spaceHtml
   [ anchor ! [href $ baseUrl ++ "/feedback" ] << "Feedback"
   ]
   
