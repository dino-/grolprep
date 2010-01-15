-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Grolprep.Web.Util
   where

import Control.Monad
import Text.XHtml.Strict

import Grolprep.Common.Util


{- The base URL of the application
-}
baseUrl :: String
baseUrl = "/grolprep/bin/fcc-grol-prep.cgi"


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

