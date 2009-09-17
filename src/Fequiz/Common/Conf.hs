-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- |
   Simple module for loading config files
-}
module Fequiz.Common.Conf
   ( ConfMap, parseToMap )
   where

import Data.Map hiding ( map )
import Data.Maybe ( catMaybes )
import Text.Regex ( matchRegex, mkRegex )


type ConfMap = Map String String


{- |
   Parse config file data into a simple (Map String String).

   For example, this:

   >  --- file start ---
   >  foo=one
   >  # a comment
   >
   >  bar
   >  baz-blorp=2
   >  --- file end ---

   becomes:

   >  fromList [("foo","one"),("bar",""),("baz-blorp","2")]

   Comments (prefixed with #) and blank lines in the config file 
   are discarded.
-}
parseToMap :: String -> ConfMap
parseToMap entireConf =
   fromList $ map (\[k, v] -> (k, v))
      $ catMaybes $ map (matchRegex re) $ lines entireConf
   where
      re = mkRegex "^([^#][^=]*)=?(.*)"