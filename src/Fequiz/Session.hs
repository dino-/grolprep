-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}


module Fequiz.Session
   ( StudyType (..), Session (..)
   , App, runApp
   , getSession, putSession, destroySession
   )
   where

import Control.Monad.State
import Data.Maybe
import Network.CGI
import Network.CGI.Monad
import Network.CGI.Protocol
import System.Directory
import System.FilePath
import System.IO

import Fequiz.Common
import Fequiz.SessionId
import Paths_fequiz


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
   , sessCurrOrd  :: [Int]
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


loadSession :: String -> IO Session
loadSession sessionId = do
   path <- getDataFileName $ "session" </> sessionId
   liftM read $ readFile path


saveSession :: String -> Session -> IO ()
saveSession sessionId session = do
   sessionDir <- getDataFileName "session"
   mkdir sessionDir

   let path = sessionDir </> sessionId
   unlink path
   writeFile path $ show session


deleteSession :: String -> IO ()
deleteSession sessionId = do
   path <- getDataFileName $ "session" </> sessionId
   unlink path


getSession :: App (Maybe Session)
getSession = do
   mbSession <- get
   when (isNothing mbSession) $ do
      mbSessionId <- getCookie appId
      case mbSessionId of
         Just sessionId -> do
            session <- liftIO $ loadSession sessionId
            put $ Just session
         Nothing -> return ()
   get


putSession :: Session -> App ()
putSession session = do
   existingCookie <- getCookie appId
   sessionId <- case existingCookie of
      Just sid -> return sid
      Nothing -> do
         ip <- remoteAddr
         sid <- liftIO $ generateSessionId ip
         let cookie = newCookie appId sid
         setCookie cookie
         return sid

   liftIO $ saveSession sessionId session
   put $ Just session


destroySession :: App ()
destroySession = do
   existingCookie <- getCookie appId
   case existingCookie of
      Just sessionId -> do
         liftIO $ deleteSession sessionId
         let cookie = newCookie appId ""
         deleteCookie cookie
      Nothing -> return ()

   put Nothing
