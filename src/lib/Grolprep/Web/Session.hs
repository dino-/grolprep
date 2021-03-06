-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}


module Grolprep.Web.Session
   ( Session (..), StudyType (..)
   , App, runApp
   , getConfig
   , getSession, putSession, destroySession
   )
   where

import Control.Monad.State
import Control.Monad.Reader
import Data.Map
import Data.Maybe
import Data.Time.Clock
import Network.CGI
import Network.CGI.Monad
import Network.CGI.Protocol
import Prelude hiding ( lookup )
import System.FilePath
import System.IO
import System.IO.Error ( tryIOError )
import Text.Printf

import Grolprep.Common.Conf
import Grolprep.Common.Log
import Grolprep.Common.Util ( appId, getVarFilePath, mkdir, unlink )
import Grolprep.Web.SessionId


data StudyType
   = StudySimulation Int      -- Which element to run sim test
   | StudyRegular Int String  -- Which element and subelement
   | StudyCustom String       -- User entered a space-delimited list 
                              --    of questions
   deriving (Read, Show)


data Session = Session
   { sessPassNumber  :: Int         -- Current pass number
   , sessPassTot     :: Int         -- Total problems when pass started
   , sessPassProbIx  :: Int         -- Current problem wrt total for pass

   , sessStudyRandA  :: Bool        -- True if we're shuffling answers
   , sessStudyList   :: [String]    -- List of remaining problem IDs
   , sessStudyProbIx :: Int         -- Problem index we're on
   , sessStudyAOrd   :: [Int]       -- Order of the answers for curr problem
   , sessStudyLastA  :: Int         -- Index of last answer chosen
   }
   deriving (Read, Show)


newtype AppT m a = App (ReaderT ConfMap (StateT (Maybe Session) (CGIT m)) a)
   deriving (Applicative, Functor, Monad, MonadIO, MonadState (Maybe Session), 
      MonadReader ConfMap)


type App a = AppT IO a


instance MonadCGI (AppT IO) where
   cgiAddHeader n v = App $ lift $ lift $ cgiAddHeader n v
   cgiGet x = App $ lift $ lift $ cgiGet x


runApp :: ConfMap -> App CGIResult -> IO ()
runApp conf (App a) = do
   env <- getCGIVars
   hRunCGI env stdin stdout (runCGIT
      (evalStateT (runReaderT a conf) Nothing))
   return ()


{- Convenience function to get the value for a specific key
   out of the config held by the reader
-}
getConfig :: (MonadReader ConfMap m) => String -> m String
getConfig key = do
   conf <- ask
   return $ maybe "" id $ lookup key conf


newGrolprepCookie :: String -> String -> IO Cookie
newGrolprepCookie aid sid = do
   let rawcookie = newCookie aid sid
   let sevenDaysInSeconds = 604800
   expiration <- addUTCTime sevenDaysInSeconds <$> getCurrentTime
   return $ rawcookie { cookiePath = Just "/" , cookieExpires = Just expiration } 


loadSession :: String -> IO Session
loadSession sessionId = do
   let path = getVarFilePath $ "session" </> sessionId
   liftM read $ readFile path


saveSession :: String -> Session -> IO ()
saveSession sessionId session = do
   let sessionDir = getVarFilePath "session"
   mkdir sessionDir

   let path = sessionDir </> sessionId
   unlink path
   writeFile path $ show session


deleteSession :: String -> IO ()
deleteSession sessionId = do
   let path = getVarFilePath $ "session" </> sessionId
   unlink path


getSession :: App (Maybe Session)
getSession = do
   mbSession <- get
   when (isNothing mbSession) $ do
      mbSessionId <- getCookie appId
      case mbSessionId of
         Just sessionId -> do
            esess <- liftIO $ tryIOError $ loadSession sessionId
            case esess of
               Right session -> put $ Just session
               Left _ -> do
                  destroySession
                  llog NOTICE $ "Stale client session cookie detected, removed. Session id: " ++ sessionId
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
         c <- liftIO $ newGrolprepCookie appId sid
         setCookie c 
         llog NOTICE $ printf "Created new session for ip: %s, session id: %s" ip sid
         return sid

   liftIO $ saveSession sessionId session
   put $ Just session


destroySession :: App ()
destroySession = do
   existingCookie <- getCookie appId
   case existingCookie of
      Just sessionId -> do
         liftIO $ deleteSession sessionId
         c <- liftIO $ newGrolprepCookie appId ""
         deleteCookie c 
         llog NOTICE $ "Destroyed session, id: " ++ sessionId
      Nothing -> return ()

   put Nothing
