#! /usr/bin/env runhaskell

-- Copyright: 2009 Ren Hoek
-- License: BSD3 (see LICENSE)
-- Author: Ren Hoek <ren@spumco.co.dk>

import Data.List
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import System.Cmd
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


main = defaultMainWithHooks (simpleUserHooks 
   { postInst = customPostInst
   } )
   where
      customPostInst _ _ _ localBuildInfo = do
         let sharePath = (fromPathTemplate $ prefix
               $ installDirTemplates localBuildInfo)
               </> "share"

         let group = distroSpecificHttpGroup sharePath

         system $ printf "chgrp -R %s %s" group sharePath
         system $ printf "chmod -R g+w %s" sharePath

         return ()
