-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Fequiz.Study
   where

import Control.Monad
import Data.List
import Data.Maybe
import Network.CGI
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


nextProblem :: Session -> IO (Maybe Problem)
nextProblem session = do
   let (Set questionsPath) = sessType session
   let scurr = sessCurr session
   let slist = sessList session

   eps <- liftM parseProblems $
      getDataFileName questionsPath >>= readFile
   let ps = either undefined snd eps

   return $ if (scurr < length slist)
      then Just (ps !! (slist !! scurr))
      else Nothing


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
            (   option ! [value "questions/element1", selected] << "Element 1 (170 questions)"
            +++ option ! [value "questions/subelement3a"] << "Subelement 3A - Operating procedures (40 questions)"
            +++ option ! [value "questions/subelement3b"] << "Subelement 3B - Radio wave propagation (42 questions)"
            +++ option ! [value "questions/subelement3c"] << "Subelement 3C - Radio practice (69 questions)"
            +++ option ! [value "questions/subelement3d"] << "Subelement 3D - Electrical principles (202 questions)"
            +++ option ! [value "questions/subelement3e"] << "Subelement 3E - Circuit components (150 questions)"
            +++ option ! [value "questions/subelement3f"] << "Subelement 3F - Practical circuits (139 questions)"
            +++ option ! [value "questions/subelement3g"] << "Subelement 3G - Signals and emissions (131 questions)"
            +++ option ! [value "questions/subelement3h"] << "Subelement 3H - Antennas and feedlines (143 questions)"
            +++ option ! [value "questions/element8"] << "Element 8 (321 questions)"
            +++ option ! [value "questions/small1"] << "small1 (6 questions)"
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

   nas <- liftIO $ orderer randA $ zip [0..] $ map extractAnswer eas

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


formAnswer :: Int -> Problem -> Html
formAnswer g (Problem _ q eas) = form << (
   [ correctness (eas !! g)
   , p ! [theclass "question"] << q
   , thediv << (ansLines eas)
   , submit (show ActAnswer) "Next question" ! [theclass "button"]
   ] )
   where
      correctness (Right _) = p ! [theclass "correct-ans"] << "CORRECT"
      correctness _         =
         p ! [theclass "incorrect-ans"] << "INCORRECT"

      ansLines eas' = map f $ zip [0..] eas'
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

   deleteSession

   formStart


actionSetupSession :: App CGIResult
actionSetupSession = do
   llog INFO "actionSetupSession"

   questionsPath <- liftM fromJust $ getInput "file"
   randA <- liftM (maybe False (const True)) $ getInput "randA"

   numQuestions <- liftIO $ do
      eps <- liftM parseProblems $
         getDataFileName questionsPath >>= readFile
      let ps = either undefined snd eps
      return $ length ps

   questionOrderer <- liftM (maybe return (const shuffle))
      $ getInput "randQ"
   questionNumbers <- liftIO $ questionOrderer [0..(numQuestions - 1)]
   let session = Session (Set questionsPath) randA 1 0
         (length questionNumbers) 0 questionNumbers
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
      (Just np, _    ) -> do
         let newSession =
               session { sessPassCurr = passCurr + 1 }
         putSession newSession
         formPoseProblem np
      (_      , True ) -> actionInitialize
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

   answer <- liftM fromJust $ readInput "answer"

   let (newCurr, newList) = case (as !! answer) of
         Right _ -> (curr, removeFromList curr list)
         Left _  -> (curr + 1, list)

   -- Make the new session and set it
   let newSession =
         session { sessCurr = newCurr , sessList = newList }
   putSession newSession

   answerPage <- liftIO $ page $ formCancel +++
      (headingStats newSession) +++ formAnswer answer problem
   output $ renderHtml answerPage
