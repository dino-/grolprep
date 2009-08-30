-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Fequiz.Shuffle
   ( shuffle )
   where

-- Clever shuffle code from dolio on #haskell, thanks!

import Control.Monad
import System.Environment
import System.Random


pick :: [a] -> IO (a, [a])
pick l = do
   r <- randomRIO (0, length l - 1)
   let (h, e:t) = splitAt r l
   return (e, h ++ t)


shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle l  = do
   (e, l') <- pick l
   (e:) `liftM` shuffle l'
