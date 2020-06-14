-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Grolprep.Web.Util
   where

import Text.XHtml.Strict

import Grolprep.Common.Util ( appVersion, getRelDataFilePath )


{- The base URL of the application
-}
baseUrl :: IO String
baseUrl = getRelDataFilePath "../bin/fcc-grol-prep.cgi"


createCssLinks :: [FilePath] -> IO [Html]
createCssLinks paths = mapM createLink ("css/common.css" : paths)
   where
      createLink path = do
         cssPath <- getRelDataFilePath path
         return $ thelink noHtml ! [href cssPath, rel "stylesheet", 
             thetype "text/css"]


titleBar :: Html
titleBar = thetitle << "GROLPrep"


heading :: IO Html
heading = do
   return $ p <<
      (thediv ! [theclass "banner"]) << (h1 ! [theclass "heading"]) << (
      "GROLPrep" +++
      (thespan ! [theclass "banner-dark-text"] << primHtml " &middot;")
      +++
      thespan ! [theclass "heading-smaller"] << " FCC Commercial Radio Operators License exam preparation"
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
   h <- heading
   return (
      (header <<
         titleBar
         +++
         cssLinks
      )
      +++
      body ! [strAttr "onLoad"
         "document.getElementById(\"enter\").focus()"] <<
         [ h
         , b
         , footer
         ] 
      )
