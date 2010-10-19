#! /usr/bin/env runhaskell

-- Copyright: 2009, 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad ( when )
import Data.List
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import System.Cmd
import System.Directory ( createDirectory, doesDirectoryExist )
import System.FilePath
import Text.Printf


distroSpecificHttpGroup path
   -- Debian-based
   | isPrefixOf "/var/www" path = "www-data"

   -- Arch Linux
   | isPrefixOf "/srv/http" path = "http"

   -- Just fail if it's not one of the above cases
   -- Add new entries for other distros as they come up
   | otherwise = undefined


fixDir group path = do
   system $ printf "chgrp -R %s %s" group path
   system $ printf "chmod -R g+w %s" path
   return ()


main = defaultMainWithHooks (simpleUserHooks 
   { postInst = customPostInst
   } )
   where
      customPostInst _ _ _ localBuildInfo = do
         let sharePath = (fromPathTemplate $ prefix
               $ installDirTemplates localBuildInfo)
               </> "share"

         let group = distroSpecificHttpGroup sharePath

         fixDir group sharePath

         let logDirPath = "/var/log/grolprep"
         needLogDir <- fmap not $ doesDirectoryExist logDirPath
         when needLogDir $ do
            createDirectory logDirPath
            fixDir group logDirPath

         return ()
