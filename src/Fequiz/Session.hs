-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}


module Fequiz.Session
   ( StudyType (..), Session (..)
   , App, runApp
   , getSession, putSession, deleteSession
   )
   where

import Control.Monad.State
import Data.Maybe
import Network.CGI
import Network.CGI.Monad
import Network.CGI.Protocol
import System.IO

import Fequiz.Common


data StudyType
   = Set String  -- A specific question set, this is the path
                 -- like "resources/3a.txt"
   -- | Sim         -- A simulation of a real 76 question test
   deriving (Read, Show)


data Session = Session
   { sessType     :: StudyType
   , sessRandA    :: Bool
   , sessPass     :: Int
   , sessPassCurr :: Int
   , sessPassTot  :: Int
   , sessCurr     :: Int
   , sessList     :: [Int]
   }
   deriving (Read, Show)


newtype AppT m a = App (StateT (Maybe Session) (CGIT m) a)
   deriving (Monad, MonadIO, MonadState (Maybe Session))


type App a = AppT IO a


instance MonadCGI (AppT IO) where
   cgiAddHeader n v = App $ lift $ cgiAddHeader n v
   cgiGet x = App $ lift $ cgiGet x


runApp :: App CGIResult -> IO ()
runApp (App a) = do
   env <- getCGIVars
   hRunCGI env stdin stdout (runCGIT (evalStateT a Nothing))
   return ()


getSession :: App (Maybe Session)
getSession = do
   ms <- get
   when (isNothing ms) $ (readCookie appId) >>= put
   get


putSession :: Session -> App ()
putSession s = do
   let cookie = newCookie appId $ show s
   setCookie cookie
   put $ Just s


deleteSession :: App ()
deleteSession = do
   let cookie = newCookie appId ""
   deleteCookie cookie
   put Nothing
