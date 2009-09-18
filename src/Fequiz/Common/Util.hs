-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Fequiz.Common.Util
   where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Time.Clock ( getCurrentTime )
import Data.Time.Format ( formatTime )
import Data.Time.LocalTime ( utcToLocalZonedTime )
import System.Directory
import System.IO
import System.Locale ( defaultTimeLocale )
import Text.Printf
import Text.Regex

import Paths_fequiz


{- Some identifying info for this application
-}
appName, appVersion, appId :: String
appName = "fequiz"
appVersion = "1.0.0.7"
appId = printf "%s-%s" appName appVersion


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


{- Make a directory if it doesn't already exist
-}
mkdir :: FilePath -> IO ()
mkdir path =
   doesDirectoryExist path >>= (flip unless $ createDirectory path)


{- Remove a file given a file path. Does nothing at all if the file
   does not exist.
-}
unlink :: FilePath -> IO ()
unlink path = doesFileExist path >>= (flip when $ removeFile path)

{- Format the time right now given a formatting string
-}
formattedDate :: String -> IO String
formattedDate formatString =
   liftM (formatTime defaultTimeLocale formatString)
      $ getCurrentTime >>= utcToLocalZonedTime
