-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Grolprep.Web.Util
   where

import Control.Monad
import Data.List
import Data.Maybe
import Network.CGI
import Text.XHtml.Strict

import Grolprep.Common.Util


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
baseUrl = "/grolprep/bin/fcc-grol-prep.cgi"


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


createCssLinks :: [FilePath] -> IO [Html]
createCssLinks paths = mapM createLink ("css/common.css" : paths)
   where
      createLink path = do
         cssPath <- getRelDataFileName path
         return $ thelink noHtml ! [href cssPath, rel "stylesheet", 
             thetype "text/css"]


titleBar :: Html
titleBar = thetitle << "GROLPrep"


heading :: Html
heading = (thediv ! [theclass "banner"]) << (h1 ! [theclass "heading"]) << (
   "GROLPrep" +++
   (thespan ! [theclass "banner-dark-text"] << primHtml " &middot;")
   +++
   thespan ! [theclass "heading-smaller"] << " FCC General Radio Operators License exam preparation"
   )


footer :: Html
footer = thediv ! [theclass "banner"] << h2 ! [theclass "footer"] << (
   "GROLPrep " +++ thespan ! [theclass "banner-dark-text"] << appVersion )


{- Convenience function to deal with some of the repetitive parts
   included in every HTML document
-}
page :: [FilePath] -> Html -> IO Html
page cssPaths b = do
   cssLinks <- createCssLinks cssPaths
   return (
      (header <<
         titleBar
         +++
         cssLinks
      )
      +++
      body <<
         [ heading
         , b
         , footer
         ] 
      )

