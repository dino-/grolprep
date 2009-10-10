-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}


module Grolprep.Web.Study
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
import System.Random
import Text.Printf
import Text.XHtml.Strict

import Grolprep.Common.Data
import Grolprep.Common.Log
import Grolprep.Common.Util
import Grolprep.Common.Shuffle
import Grolprep.Web.Session
import Grolprep.Web.Util
import Paths_grolprep


{- Dispatches HTML requests for study URLs
-}
dispatchStudy :: App CGIResult
dispatchStudy = do
   mbSession <- getSession
   --llog DEBUG $ "session: " ++ show mbSession

   -- Figure out which form button was used for submit
   mbForm <- getButtonPressed
   llog DEBUG $ "form button: " ++ show mbForm

   -- Map session status and form button pressed into actions
   case (mbSession, mbForm) of
      (Nothing, Nothing       ) -> actionInitialize
      (Nothing, Just ActStart ) -> actionSetupSession
      (_,       Just ActPose  ) -> actionCorrectProblem
      (_,       Just ActQuit  ) -> actionInitialize
      (Just _,  _             ) -> actionNextProblem
      (_,       _             ) -> actionInitialize


{- Remove an indexed element from a list
-}
removeFromList :: Int -> [a] -> [a]
removeFromList i xs = take i xs ++ drop (i + 1) xs


dbPath :: IO FilePath
dbPath = getDataFileName $ "grolprep" <.> "sqlite"


getSimProblemIds :: Int -> IO [ProblemId]
getSimProblemIds element = do
   conn <- dbPath >>= connectSqlite3

   -- Get all the keytopics for the specified element
   keytopics <- liftM concat $
      quickQuery' conn ( unlines
         [ "SELECT id FROM keytopic WHERE "
         , "   element=?"
         , "   ORDER BY id"
         , ";"
         ] )
         [toSql element]

   -- Get all the question ids for these keytopics
   allProbIds <- mapM 
      ( \keytopic -> liftM concat $ 
         quickQuery' conn ( unlines
            [ "SELECT id FROM problem WHERE "
            , "   element=? AND"
            , "   keytopic=?"
            , ";"
            ] )
         [toSql element, keytopic] 
      )
      keytopics

   disconnect conn

   -- Reduce that list of lists down to one randomly-selected item
   -- from each inner list
   randProbIdsSV <- mapM
      (\is -> do
         idx <- randomRIO (0, (length is) - 1)
         return $ is !! idx
      )
      allProbIds

   -- Convert this last list from [SqlValue] to [ProblemId]
   -- and return
   return $ map fromSql randProbIdsSV


getRegularProblemIds :: Int -> String -> IO [ProblemId]
getRegularProblemIds element subelement = do
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
formCancel = form ! [ method "POST", action $ baseUrl ] << (
   submit (show ActQuit) "Cancel test session" ! [theclass "button"]
   )


headingStats :: Session -> Html
headingStats session =
   p ! [theclass "question"] <<
      (printf "Pass %d, question %d of %d total in this pass"
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
   (rs13, rs8) <- liftIO $ do
      conn <- dbPath >>= connectSqlite3

      stmt1 <- prepare conn $ unlines
         [ "SELECT id, element, desc FROM subelement "
         , "   WHERE element = ? or element = ?"
         , "   ORDER BY element, id"
         , ";"
         ]
      execute stmt1 [toSql (1::Int), toSql (3::Int)]
      rs13' <- fetchAllRowsMap' stmt1

      stmt2 <- prepare conn $ unlines
         [ "SELECT id, element, desc FROM subelement "
         , "   WHERE element = ?"
         , "   ORDER BY element, id"
         , ";"
         ]
      execute stmt2 [toSql (8::Int)]
      rs8' <- fetchAllRowsMap' stmt2

      disconnect conn
      return (rs13', rs8')

   startPage <- liftIO $ do
      scriptPath <- getRelDataFileName "scripts/formStart.js"
      cssLinks <- createCssLinks [ "css/setup.css" ]
      return (
         (header <<
            titleBar
            +++
            cssLinks
            +++
            script ! [src scriptPath] << noHtml
         )
         +++
         body ! [strAttr "onload" "populateQuestionsList()"] <<
            ([heading, about, (theform rs13 rs8)] 
             +++
             thediv ! [theclass "banner"] << h2 ! [theclass "footer"] << (
               "GROLPrep " +++
               thespan ! [theclass "banner-dark-text"] << appVersion
               +++
               thespan ! [theclass "feedback"] << (
                  anchor ! [theclass "feedback", href "http://ui3.info/d/proj/grolprep"] << "developer site"
                  +++
                  (thespan ! [theclass "banner-dark-text"] << primHtml " &middot; ")
                  +++
                  feedback
                  )
               )
            )
         )
   output $ renderHtml startPage

   where
      constructSimOptionJs n =
         printf "         new Option('%s', '%s', false, false),"
            ((printf "Simulate Element %d exam" n) :: String)
            (show $ StudySimulation n)

      constructNormalOptionJs rsMap =
         printf "         new Option('%s', '%s', false, false),"
            seDesc (show $ StudyRegular elValue seValue)
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

      about = 
         thediv ! [ strAttr "class" "features" ] << ( 
            p ! [ strAttr "class" "features" ] << "Features of GROLPrep" 
            +++ ulist ! [ strAttr "class" "features" ] <<
               [ li ! [theclass "features"] << "Questions and answers can be presented in a random order"
               , li ! [theclass "features"] << "Drills you on questions answered incorrectly with additional passes"
               , li ! [theclass "features"] << "Simulate complete examinations"
               , li ! [theclass "features"] << "Study specific subelements"
               ] )
         +++
         h3 << 
            (
            "This site provides a study tool and test simulator for the FCC GROL and Radar Endorsement examinations. GROLPrep is used by students of the " 
            +++ anchor ! [href "http://www.burlingtonaviationtech.org" ] 
               << "Burlington Aviation Technology at the Vermont Aircraft Maintenance School"
            +++ "."
            )
         +++ 
         h3 << ("The source test questions can be acquired from the "
            +++ (anchor ! [href "http://wireless.fcc.gov/commoperators/index.htm?job=question_pools" ]  << "FCC Commercial Radio Operators License" )
            +++ " site.")


      theform rs13' rs8' =
         let study13 = "study13"
             study8  = "study8"
         in thediv << (
         script << primHtml (
            "\n      var questionOpts13 = [\n" ++
            (unlines (map constructSimOptionJs [1, 3])) ++
            (unlines (map constructNormalOptionJs rs13')) ++ "      ];" ++

            "\n      var questionOpts8 = [\n" ++
            (constructSimOptionJs 8) ++
            (unlines (map constructNormalOptionJs rs8')) ++ "      ];"
         )
         +++
         form ! [ identifier "setupForm", method "POST", action $ baseUrl ++ "/study" ] <<
            (
            p << "Let's get started! Please select type of study"
            +++
            fieldset << (
               (radio "" study13 ! [name "studyType", strAttr "id" study13, checked , strAttr "onclick" "populateQuestionsList()"]
                  +++ label ! [thefor study13] << "GROL (Elements 1 and 3)")
               +++
               (radio "" study8 ! [name "studyType", strAttr "id" study8, strAttr "onclick" "populateQuestionsList()"]
                  +++ label ! [thefor study8] << "Radar Endorsement (Element 8)")
               +++
               p << select ! [identifier "questions", name "questions", size "12"] << noHtml
            )
            +++ p << (
               checkbox "randQ" "" +++
               label ! [thefor "randQ"] << "Ask the questions in a random order"
               )
            +++ p << (checkbox "randA" "" +++
               label ! [thefor "randA"] << "Randomly order the answers of each question"
               )
            +++ p << submit (show ActStart) "Start study session" ! [theclass "button"]
            )
         )


feedback :: Html
feedback = thespan <<
   anchor ! [theclass "feedback", href $ baseUrl ++ "/feedback"]
      << "feedback"


getProblemMetaInfo :: String -> 
   IO ((Int, String), (String, String), (Int, String))
getProblemMetaInfo problemId = do
   conn <- dbPath >>= connectSqlite3

   stmt <- prepare conn $ unlines
         [ "SELECT element, subelement, keytopic "
         , "   FROM problem "
         , "   WHERE "
         , "      id=?"
         , ";"
         ]
   execute stmt [toSql problemId]
   pRs <- liftM head $ fetchAllRowsMap' stmt

   let sEl = fromJust $ lookup "element" pRs
   let sSe = fromJust $ lookup "subelement" pRs
   let sKt = fromJust $ lookup "keytopic" pRs

   (elDesc:_) <- liftM concat $
      quickQuery' conn ( unlines
         [ "SELECT desc FROM element WHERE "
         , "   id=?"
         , ";"
         ] )
         [sEl]

   (seDesc:_) <- liftM concat $
      quickQuery' conn ( unlines
         [ "SELECT desc FROM subelement WHERE "
         , "   id=? AND element=?"
         , ";"
         ] )
         [sSe, sEl]

   (ktDesc:_) <- liftM concat $
      quickQuery' conn ( unlines
         [ "SELECT desc FROM keytopic WHERE "
         , "   id=? AND element=? AND subelement=?"
         , ";"
         ] )
         [sKt, sEl, sSe]

   disconnect conn

   return
      ( (fromSql sEl, fromSql elDesc)
      , (fromSql sSe, fromSql seDesc)
      , (fromSql sKt, fromSql ktDesc)
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
   mi <- liftIO metaInfo
   posePage <- liftIO $ page [] $ formCancel +++ 
      mi +++ fpp +++ (headingStats session)
   output $ renderHtml posePage

   where
      orderer True  = shuffle
      orderer False = return

      metaInfo = do
         ((_, elDesc), (seId, seDesc), (ktId, ktDesc))
            <- getProblemMetaInfo pid

         return $
            [ p << ((printf "%s" elDesc) :: String)
            , p << ((printf "Subelement %s: %s, Key Topic %d: %s"
                        seId seDesc ktId ktDesc) :: String)
            ]

      formPoseProblem' q' as = form ! [ method "POST", action $ baseUrl ++ "/study" ] << (
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

   mi <- liftIO metaInfo
   answerPage <- liftIO $ page [] $ formCancel +++
      mi +++ (theform nas) +++ (headingStats session)
   output $ renderHtml answerPage

   where
      metaInfo = do
         ((_, elDesc), (seId, seDesc), (ktId, ktDesc))
            <- getProblemMetaInfo pid

         return $
            [ p << ((printf "%s" elDesc) :: String)
            , p << ((printf "Subelement %s: %s, Key Topic %d: %s"
                        seId seDesc ktId ktDesc) :: String)
            ]

      theform nas' = form ! [ method "POST", action $ baseUrl ++ "/study" ] << (
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
      Just (StudySimulation element) -> do
         -- Leaving much of this randomization code alone for now
         -- until we hear how the simulation tests are to work

         randA <- liftM (maybe False (const True)) $ getInput "randA"

         problems <- liftIO $ getSimProblemIds element

         questionOrderer <- liftM (maybe return (const shuffle))
            $ getInput "randQ"

         sortedProblems <- liftIO $ questionOrderer problems

         putSession $ Session
               { sessRandA    = randA
               , sessPass     = 1
               , sessPassCurr = 0
               , sessPassTot  = length sortedProblems
               , sessCurr     = 0
               , sessCurrOrd  = []
               , sessList     = sortedProblems
               }

         actionNextProblem

      Just (StudyRegular element subelement) -> do
         randA <- liftM (maybe False (const True)) $ getInput "randA"

         problems <- liftIO $ getRegularProblemIds element subelement

         questionOrderer <- liftM (maybe return (const shuffle))
            $ getInput "randQ"

         sortedProblems <- liftIO $ questionOrderer problems

         putSession $ Session
               { sessRandA    = randA
               , sessPass     = 1
               , sessPassCurr = 0
               , sessPassTot  = length sortedProblems
               , sessCurr     = 0
               , sessCurrOrd  = []
               , sessList     = sortedProblems
               }

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
         putSession $ session { sessPassCurr = passCurr + 1 }
         formPoseProblem np

      -- No next problem and there are no more
      -- not-correctly-answered. We're done.
      (_      , True ) -> actionInitialize

      -- No next problem for this pass, but not-correctly-answered
      -- problems remain. Start next pass.
      (_      , False) -> do
         let pass = sessPass session
         putSession $ session
               { sessPass = pass + 1
               , sessPassCurr = 0
               , sessPassTot = (length list)
               , sessCurr = 0
               }

         actionNextProblem


actionCorrectProblem :: App CGIResult
actionCorrectProblem = do
   llog INFO "actionCorrectProblem"

   -- Get current session and extract some things from it
   mbSession <- getSession

   -- Pull the answer they selected out of the form
   mbAnswer <- readInput "answer"

   case (mbSession, mbAnswer) of
      -- We have a session and the user answered, correct it
      (Just session, Just answer) -> do
         let curr = sessCurr session
         let list = sessList session

         -- Evaluate the user's answer
         mbnp <- liftIO $ nextProblem session
         let problem@(Problem _ _ as) = fromJust mbnp

         -- Reconstitute the order of the answers from when the question
         -- was asked. We cleverly stored this in the session.
         let currOrd = sessCurrOrd session
         let ast = combineIxAndAns currOrd as

         let (newCurr, newList) = case (snd $ ast !! answer) of
               Right _ -> (curr, removeFromList curr list)
               Left _  -> (curr + 1, list)

         -- Make the new session and set it
         putSession $ session { sessCurr = newCurr , sessList = newList }

         formAnswer answer problem

      -- We have no session and yet they somehow arrived at this form
      -- Get out of here and go back to the beginning
      (Nothing, _) -> actionInitialize

      -- The user has submit the form with no answer selected
      -- Kick them right back to the pose problem form
      (Just session, _) -> do
         mbnp <- liftIO $ nextProblem session
         formPoseProblem $ fromJust mbnp
