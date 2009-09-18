-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}


module Fequiz.Web.Session
   ( Session (..), StudyType (..)
   , App, runApp
   , getSession, putSession, destroySession
   )
   where

import Control.Monad.State
import Data.Maybe
import Network.CGI
import Network.CGI.Monad
import Network.CGI.Protocol
import System.FilePath
import System.IO
import System.IO.Error

import Fequiz.Common.Log
import Fequiz.Common.Util
import Fequiz.Web.SessionId
import Paths_fequiz


data StudyType
   = StudySimulation Int      -- Which element to run sim test
   | StudyRegular Int String  -- Which element and subelement
   deriving (Read, Show)


data Session = Session
   { sessRandA    :: Bool
   , sessPass     :: Int
   , sessPassCurr :: Int
   , sessPassTot  :: Int
   , sessCurr     :: Int
   , sessCurrOrd  :: [Int]
   , sessList     :: [String]
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


newFequizCookie :: String -> String -> Cookie
newFequizCookie aid sid = do
   let rawcookie = newCookie aid sid
   rawcookie { cookiePath = Just "/" } 

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
            esess <- liftIO $ try $ loadSession sessionId
            case esess of
               Right session -> put $ Just session
               Left _ -> do
                  llog INFO
                     "stale client session cookie detected, removed"
                  destroySession
                  return ()
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
         setCookie $ newFequizCookie appId sid
         return sid

   liftIO $ saveSession sessionId session
   put $ Just session


destroySession :: App ()
destroySession = do
   existingCookie <- getCookie appId
   case existingCookie of
      Just sessionId -> do
         liftIO $ deleteSession sessionId
         deleteCookie $ newFequizCookie appId ""
      Nothing -> return ()

   put Nothing
