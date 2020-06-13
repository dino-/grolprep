{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Grolprep.Common.Shuffle
   ( shuffle )
   where

-- Clever shuffle code from dolio on #haskell, thanks!

import System.Random ( randomRIO )


pick :: [a] -> IO (a, [a])
pick l = do
   r <- randomRIO (0, length l - 1)
   -- This let binding is the incomplete-uni-patterns. We can see below in
   -- shuffle that no empty list will ever be passed to pick. Turning off this
   -- warning in this source file is safe.
   let (h, e:t) = splitAt r l
   return (e, h ++ t)


shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle l  = do
   (e, l') <- pick l
   (e:) <$> shuffle l'
