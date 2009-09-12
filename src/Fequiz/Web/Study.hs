-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}


module Fequiz.Web.Study
   where

import Control.Monad
import Data.Convertible.Base
import Data.List hiding ( lookup )
import Data.Map ( Map, lookup )
import Data.Maybe
import Database.HDBC
import Database.HDBC.Sqlite3
import Network.CGI
import Prelude hiding ( lookup )
import System.FilePath
import System.Log
import Text.Printf
import Text.XHtml.Strict

import Fequiz.Common.Data
import Fequiz.Common.Log
import Fequiz.Common.Shuffle
import Fequiz.Common.Util
import Fequiz.Web.Session
import Paths_fequiz


{- Remove an indexed element from a list
-}
removeFromList :: Int -> [a] -> [a]
removeFromList i xs = take i xs ++ drop (i + 1) xs


dbPath :: IO FilePath
dbPath = getDataFileName $ "fequiz" <.> "sqlite"


getProblemIds :: Int -> String -> IO [ProblemId]
getProblemIds element subelement = do
   conn <- dbPath >>= connectSqlite3
   stmt <- prepare conn $ unlines
      [ "SELECT id FROM problem WHERE"
      , "   element=? AND"
      , "   subelement=?"
      , ";"
      ]

   execute stmt [toSql element, toSql subelement]
   rs <- sFetchAllRows' stmt
   disconnect conn
   return $ map fromJust $ concat rs


{- This function is used to take a list of randomized indexes to
   answers, and a list of the answers themselves, combine them into
   tuples, and sort that new list on the index values.
-}
combineIxAndAns :: (Ord a) => [a] -> [b] -> [(a, b)]
combineIxAndAns xs ys =
   sortBy (\x y -> compare (fst x) (fst y)) $ zip xs ys


nextProblem :: Session -> IO (Maybe Problem)
nextProblem session = do
   let scurr = sessCurr session
   let slist = sessList session

   if (scurr < length slist)
      then do
         conn <- dbPath >>= connectSqlite3
         stmt <- prepare conn $ unlines
            [ "SELECT probdata FROM problem WHERE "
            , "   id=?"
            , ";"
            ]
         execute stmt [toSql $ slist !! scurr]
         rs <- sFetchAllRows' stmt
         disconnect conn

         return $ maybe Nothing (Just . read) $ head $ concat rs
      else return Nothing


{- HTML pages and forms 
-}

formCancel :: Html
formCancel = form << (
   submit (show ActQuit) "Cancel test session" ! [theclass "button"]
   )


headingStats :: Session -> Html
headingStats session =
   p << (printf "Pass %d, question %d of %d total in this pass"
      pass passCurr passTot :: String)
   +++
   p << (printf "%d (%0.1f%%) correct so far for this pass"
      correct perc :: String)

   where
      pass = sessPass session
      passCurr = sessPassCurr session
      passTot = sessPassTot session
      list = sessList session

      correct = passTot - (length list)

      perc :: Float
      perc = (fromIntegral correct / fromIntegral passTot) * 100


lookupSqlValue ::
   (  Data.Convertible.Base.Convertible SqlValue a
   ,  Ord k )
   => k
   -> Map k SqlValue
   -> Maybe a
lookupSqlValue key mp = maybe Nothing fromSql $ lookup key mp


formStart :: App CGIResult
formStart = do
   -- Retrieve the subelement info from db
   rsSe <- liftIO $ do
      conn <- dbPath >>= connectSqlite3
      stmt <- prepare conn $ unlines
         [ "SELECT id, element, desc FROM subelement "
         , "   ORDER BY element, id"
         , ";"
         ]

      execute stmt []
      rs <- fetchAllRowsMap' stmt
      disconnect conn
      return rs

   startPage <- liftIO $ page $ theform rsSe
   output $ renderHtml startPage

   where
      constructOption rsMap =
         option ! [value (show (elValue, seValue))] << seDesc
         where
            seValue :: String
            seValue = fromJust $ lookupSqlValue "id" rsMap

            elValue :: Int
            elValue = fromJust $ lookupSqlValue "element" rsMap

            descValue :: String
            descValue = fromJust $ lookupSqlValue "desc" rsMap

            seDesc :: String
            seDesc = printf "Element %d, Subelement %s: %s"
               elValue seValue descValue

      theform rsSe' = form <<
         fieldset << (
         legend << "Please select type of study"
         +++
         p << select ! [name "questions", size "12"] <<
            ( map constructOption rsSe' )
         +++ p << (
            checkbox "randQ" "" +++
            label ! [thefor "randQ"] << "Ask the questions in a random order"
            )
         +++ p << (checkbox "randA" "" +++
            label ! [thefor "randA"] << "Randomly order the answers of each question"
            )
         +++ p << submit (show ActStart) "Start study session" ! [theclass "button"]
         )


formPoseProblem :: Problem -> App CGIResult
formPoseProblem (Problem pid q eas) = do
   llog INFO "formPoseProblem"

   session <- liftM fromJust getSession
   let randA = sessRandA session

   qord <- liftIO $ orderer randA [0..3]
   putSession $ session { sessCurrOrd = qord }
   let nas = combineIxAndAns qord $ map extractAnswer eas

   let fpp = formPoseProblem' q nas
   posePage <- liftIO $ page $ formCancel +++ 
      (headingStats session) +++ fpp
   output $ renderHtml posePage

   where
      orderer True  = shuffle
      orderer False = return

      formPoseProblem' q' as = form << (
         [ p ! [theclass "question"] << (pid ++ ": " +++ (primHtml q'))
         , thediv << (ansControls as)
         , submit (show ActPose) "Proceed" ! [theclass "button"]
         ] )
         where
            ansControls as' = map f as'
               where
                  f :: (Int, String) -> Html
                  f (n', a) =
                     -- we pass empty string to radio so we can explictly set different name and id attributes 
                     p << ((radio "" (show n') ! [theclass "hanging", name "answer", strAttr "id" (show n')])
                          +++ label ! [thefor (show n')] << (primHtml a))

formAnswer :: Int -> Problem -> App CGIResult
formAnswer g (Problem pid q eas) = do
   session <- liftM fromJust getSession
   let qord = sessCurrOrd session

   let nas = combineIxAndAns qord eas

   answerPage <- liftIO $ page $ formCancel +++
      (headingStats session) +++ theform nas
   output $ renderHtml answerPage

   where
      theform nas' = form << (
         [ correctness (snd $ nas' !! g)
         , p ! [theclass "question"] << (pid ++ ": " +++ (primHtml q))
         , thediv << ansLines
         , submit (show ActAnswer) "Next question" ! [theclass "button"]
         ] )
         where
            correctness (Right _) =
               p ! [theclass "correct-ans"] << "CORRECT"
            correctness _         =
               p ! [theclass "incorrect-ans"] << "INCORRECT"

            ansLines = map f nas'
               where
                  f :: (Int, Either String String) -> Html
                  f (n', Left  a)
                     | n' == g    =
                           p ! [theclass "incorrect-ans"] << (primHtml a)
                     | otherwise  = p << (primHtml a)
                  f (_ , Right a) =
                           p ! [theclass "correct-ans"] << (primHtml a)


{- Action handlers
-}

actionInitialize :: App CGIResult
actionInitialize = do
   llog INFO "actionInitialize"

   destroySession

   formStart


actionSetupSession :: App CGIResult
actionSetupSession = do
   llog INFO "actionSetupSession"

   mbQuestionsChoice <- readInput "questions"
   case mbQuestionsChoice of
      Just (element, subelement) -> do
         randA <- liftM (maybe False (const True)) $ getInput "randA"

         problems <- liftIO $ getProblemIds element subelement

         questionOrderer <- liftM (maybe return (const shuffle))
            $ getInput "randQ"

         sortedProblems <- liftIO $ questionOrderer problems

         let session = Session
               { sessRandA    = randA
               , sessPass     = 1
               , sessPassCurr = 0
               , sessPassTot  = length sortedProblems
               , sessCurr     = 0
               , sessCurrOrd  = []
               , sessList     = sortedProblems
               }
         putSession session

         actionNextProblem

      -- No questions were selected, loop back to the beginning
      Nothing -> actionInitialize


actionNextProblem :: App CGIResult
actionNextProblem = do
   llog INFO "actionNextProblem"

   session <- liftM fromJust getSession

   let passCurr = sessPassCurr session
   let list = sessList session

   mbnp <- liftIO $ nextProblem session

   case (mbnp, null list) of
      -- We have a next problem, let's get to it
      (Just np, _    ) -> do
         let newSession =
               session { sessPassCurr = passCurr + 1 }
         putSession newSession
         formPoseProblem np

      -- No next problem and there are no more
      -- not-correctly-answered. We're done.
      (_      , True ) -> actionInitialize

      -- No next problem for this pass, but not-correctly-answered
      -- problems remain. Start next pass.
      (_      , False) -> do
         let pass = sessPass session
         let newSession = session
               { sessPass = pass + 1
               , sessPassCurr = 0
               , sessPassTot = (length list)
               , sessCurr = 0
               }
         putSession newSession

         actionNextProblem


actionCorrectProblem :: App CGIResult
actionCorrectProblem = do
   llog INFO "actionCorrectProblem"

   -- Get current session and extract some things from it
   session <- liftM fromJust getSession
   let curr = sessCurr session
   let list = sessList session

   -- Evaluate the user's answer
   mbnp <- liftIO $ nextProblem session
   let problem@(Problem _ _ as) = fromJust mbnp

   -- Reconstitute the order of the answers from when the question
   -- was asked. We cleverly stored this in the session.
   let currOrd = sessCurrOrd session
   let ast = combineIxAndAns currOrd as

   answer <- liftM fromJust $ readInput "answer"

   let (newCurr, newList) = case (snd $ ast !! answer) of
         Right _ -> (curr, removeFromList curr list)
         Left _  -> (curr + 1, list)

   -- Make the new session and set it
   let newSession =
         session { sessCurr = newCurr , sessList = newList }
   putSession newSession

   formAnswer answer problem
