-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Grolprep.Common.Util
   ( appId, appVersion
   , getDataFilePath, getRelDataFilePath
   , getVarFilePath
   , mkdir, unlink
   , formattedDate
   )
   where

import Control.Monad
import Data.Time.Clock ( getCurrentTime )
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Data.Time.LocalTime ( utcToLocalZonedTime )
import Data.Version ( showVersion )
import HSInstall ( getRsrcDir )
import System.Directory
import System.FilePath ( (</>) )
import System.Posix.Files
   ( groupModes, ownerModes, setFileMode, unionFileModes )
import Text.Printf
import Text.Regex

import Paths_grolprep ( getDataDir, version )


{- Some identifying info for this application
-}
appVersion, appId :: String
appVersion = showVersion version
appId = printf "grolprep-%s" appVersion


getDataFilePath :: FilePath -> IO FilePath
getDataFilePath pathTail = (</> pathTail) <$> getRsrcDir getDataDir


{- This function allows us to rely on the cabal data-files mechanism 
   to generate relative URLs for HTML pages.

   It's a bit of a hack, but this returns the part of the path starting 
   with the share directory and continuing to the end.
-}
getRelDataFilePath :: FilePath -> IO FilePath
getRelDataFilePath pathTail = do
   fullPath <- getDataFilePath pathTail
   return $ maybe pathTail head $
      matchRegex (mkRegex "(/grolprep.*)$") fullPath


getVarFilePath :: FilePath -> FilePath
getVarFilePath pathTail = "/var" </> "local" </> "grolprep" </> pathTail


{- Make a directory if it doesn't already exist
-}
mkdir :: FilePath -> IO ()
mkdir path = do
   dirExists <- doesDirectoryExist path
   unless dirExists $ do
      createDirectory path
      setFileMode path $ ownerModes `unionFileModes` groupModes


{- Remove a file given a file path. Does nothing at all if the file
   does not exist.
-}
unlink :: FilePath -> IO ()
unlink path = doesFileExist path >>= (flip when $ removeFile path)


{- Format the time right now given a formatting string
-}
formattedDate :: String -> IO String
formattedDate formatString' =
   liftM (formatTime defaultTimeLocale formatString')
      $ getCurrentTime >>= utcToLocalZonedTime
