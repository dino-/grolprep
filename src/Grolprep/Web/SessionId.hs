-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Grolprep.Web.SessionId
   ( generateSessionId
   )
   where


import Codec.Utils
import Control.Arrow
import Control.Monad
import Data.Bits
import Data.Digest.MD5
import Data.List
import Data.Time
import Data.Time.Clock.POSIX
import Text.Printf


{- Expects a dotted quad IP address string and will compute, from that
   and the current time, a hex session ID string. This is to be used
   as the cookie value for the session.
-}
generateSessionId :: String -> IO String
generateSessionId ip = do
   -- Turn the IP address string into an Integer
   let i = inetIton ip

   -- Get the time right now in seconds since the epoch
   e <- epoch

   -- Compute the MD5 hash from the sum of these guys
   let d = computeMD5 (i + e)

   -- Convert the resulting Integer to hex and back into IO it goes
   return $ printf "%x" d


{- The time right now in seconds since the epoch
-}
epoch :: IO Integer
epoch = liftM (truncate . utcTimeToPOSIXSeconds) getCurrentTime


{- Compute the MD5 hash of an Integer
-}
computeMD5 :: Integer -> Integer
computeMD5 = fromOctets base . hash . toOctets base
   where base = 10 :: Integer


{- Convert an Integer to a dotted quad IP address string
-}
{- For completeness, but never used. Put these in a library sometime

inetNtoi :: Integer -> String
inetNtoi = intercalate "." . map show . reverse . inetNtoi'
   where
      inetNtoi' 0 = []
      inetNtoi' n = r : inetNtoi' w where (w, r) = divMod n 256
-}


{- Convert a dotted quad IP address string to an Integer
-}
inetIton :: String -> Integer

-- This is an ugly, ugly fix for machines reporting localhost with IPv6
-- We've observed Ubuntu doing this lately.
inetIton "::1" = inetIton "127.0.0.1"

inetIton ip = foldl1 f $ map read $ split '.' ip
   where
      f n r = (shiftL n 8) + r


{- Split a list given a delimiter element, excluding the delimiter

   example: split '.' "192.168.0.1" -> ["192", "168", "0", "1"]
-}
split :: (Eq a) => a -> [a] -> [[a]]
split delim = takeWhile (not . null) . 
   unfoldr (Just . (second $ drop 1) . break (== delim))
