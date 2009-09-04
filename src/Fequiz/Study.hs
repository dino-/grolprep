-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Fequiz.Study
   where

import Control.Monad
import Data.List
import Data.Maybe
import Network.CGI
import System.FilePath
import System.Log
import Text.Printf
import Text.XHtml.Strict

import Fequiz.Common
import Fequiz.Data
import Fequiz.Log
import Fequiz.Session
import Fequiz.Shuffle
import Paths_fequiz


{- Remove an indexed element from a list
-}
removeFromList :: Int -> [a] -> [a]
removeFromList i xs = take i xs ++ drop (i + 1) xs


loadQuestionData :: String -> IO [Problem]
loadQuestionData questionsName = do
   eps <- liftM parseProblems $ getDataFileName
      ("questions" </> questionsName) >>= readFile
   return $ either undefined snd eps


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
         let (questionsName, questionNum) = slist !! scurr
         ps <- loadQuestionData questionsName
         return $ Just (ps !! questionNum)
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


formStart :: App CGIResult
formStart = do
   startPage <- liftIO $ page theform
   output $ renderHtml startPage

   where
      theform = form <<
         fieldset << (
         legend << "Please select type of study"
         +++
         p << select ! [name "file", size "12"] <<
            (   option ! [value "element1", selected] << "Element 1 (170 questions)"
            +++ option ! [value "subelement3a"] << "Subelement 3A - Operating procedures (40 questions)"
            +++ option ! [value "subelement3b"] << "Subelement 3B - Radio wave propagation (42 questions)"
            +++ option ! [value "subelement3c"] << "Subelement 3C - Radio practice (69 questions)"
            +++ option ! [value "subelement3d"] << "Subelement 3D - Electrical principles (202 questions)"
            +++ option ! [value "subelement3e"] << "Subelement 3E - Circuit components (150 questions)"
            +++ option ! [value "subelement3f"] << "Subelement 3F - Practical circuits (139 questions)"
            +++ option ! [value "subelement3g"] << "Subelement 3G - Signals and emissions (131 questions)"
            +++ option ! [value "subelement3h"] << "Subelement 3H - Antennas and feedlines (143 questions)"
            +++ option ! [value "element8"] << "Element 8 (321 questions)"
            +++ option ! [value "small1"] << "small1 (6 questions)"
            )
         +++ p << (
            checkbox "randQ" "" +++
            label << "Ask the questions in a random order"
            )
         +++ p << (checkbox "randA" "" +++
            label << "Randomly order the answers of each question"
            )
         +++ p << submit (show ActStart) "Start study session" ! [theclass "button"]
         )


formPoseProblem :: Problem -> App CGIResult
formPoseProblem (Problem _ q eas) = do
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
         [ p ! [theclass "question"] << q'
         , thediv << (ansControls as)
         , submit (show ActPose) "Proceed" ! [theclass "button"]
         ] )
         where
            ansControls as' = map f as'
               where
                  f :: (Int, String) -> Html
                  f (n', a) =
                     p << ((radio "answer" (show n') ! [theclass "hanging"])
                          +++ label << a)


formAnswer :: Int -> Problem -> App CGIResult
formAnswer g (Problem _ q eas) = do
   session <- liftM fromJust getSession
   let qord = sessCurrOrd session

   let nas = combineIxAndAns qord eas

   answerPage <- liftIO $ page $ formCancel +++
      (headingStats session) +++ theform nas
   output $ renderHtml answerPage

   where
      theform nas' = form << (
         [ correctness (snd $ nas' !! g)
         , p ! [theclass "question"] << q
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
                     | n' == g    = p ! [theclass "incorrect-ans"] << a
                     | otherwise  = p << a
                  f (_ , Right a) = p ! [theclass "correct-ans"] << a


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

   questionsName <- liftM fromJust $ getInput "file"
   randA <- liftM (maybe False (const True)) $ getInput "randA"

   numQuestions <- liftIO $ do
      ps <- loadQuestionData questionsName
      return $ length ps

   questionOrderer <- liftM (maybe return (const shuffle))
      $ getInput "randQ"
   questionNumbers <- liftIO $ questionOrderer [0..(numQuestions - 1)]
   let qNameNumPairs = zip (repeat questionsName) questionNumbers
   let session = Session
         { sessRandA    = randA
         , sessPass     = 1
         , sessPassCurr = 0
         , sessPassTot  = length questionNumbers
         , sessCurr     = 0
         , sessCurrOrd  = []
         , sessList     = qNameNumPairs
         }
   putSession session

   actionNextProblem


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
