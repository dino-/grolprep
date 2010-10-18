#! /usr/bin/env runhaskell

import Control.Arrow ( (&&&) )
import Data.Map ( Map (..), fromList )
import qualified Data.Map ( lookup )
import Data.Maybe ( fromJust )
import Prelude hiding ( lookup )
import System.Environment ( getEnv )
import System.Process ( system )
import Text.Printf ( printf )


parseToMap :: [String] -> Map String String
parseToMap = fromList .
   map ((takeWhile notSpace) &&& (tail . dropWhile notSpace))
   where notSpace = (/= ' ')


mkLookup :: (Ord k) => Map k a -> k -> a
mkLookup m = \k -> fromJust $ Data.Map.lookup k m


main :: IO ()
main = do
   homeDir <- getEnv "HOME"
   ls <- fmap lines $ readFile $ homeDir ++ "/.ncftp/batdev.cfg"

   let lookup = mkLookup . parseToMap $ ls

   let cl = printf "ncftp ftp://%s:%s@%s" (lookup "user")
         (lookup "pass") (lookup "host")

   system cl

   return ()
