-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}


module Grolprep.Web.Study
   where

import Control.Monad
import Data.Char
import Data.Convertible.Base
import Data.List hiding ( lookup )
import Data.List.Split
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


{- Remove an indexed element from a list
-}
removeFromList :: Int -> [a] -> [a]
removeFromList i xs = take i xs ++ drop (i + 1) xs


dbPath :: IO FilePath
dbPath = getDataFileName $ "grolprep" <.> "sqlite"


imgRegion :: Maybe FilePath -> IO Html
imgRegion Nothing = return noHtml
imgRegion (Just im) = do
   path <- getRelDataFileName $ "figures" </> im <.> "png"
   return $ image ! [theclass "problem", src path]


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


currentProblem :: Session -> IO (Maybe Problem, Maybe String)
currentProblem session = do
   let probIx = sessStudyProbIx session
   let probIds = sessStudyList session

   if (probIx < length probIds)
      then do
         conn <- dbPath >>= connectSqlite3
         stmt <- prepare conn $ unlines
            [ "SELECT p.probdata, f.figure "
            , "   FROM problem p "
            , "   LEFT JOIN figure f "
            , "   ON p.id = f.id "
            , "   WHERE "
            , "      p.id=?"
            , ";"
            ]
         execute stmt [toSql $ probIds !! probIx]
         rs <- liftM concat $ sFetchAllRows' stmt
         disconnect conn

         let mbProblem = maybe Nothing (Just . read) $ head rs
         return (mbProblem, last rs)
      else return (Nothing, Nothing)


orderer :: Bool -> [a] -> IO [a]
orderer True  = shuffle
orderer False = return


{- HTML pages and forms 
-}

formCancel :: Html
formCancel = form !
   [ method "GET"
   , action $ baseUrl ++ "/init"
   ] << (
   submit (show ActQuit) "Cancel test session" ! [theclass "button"]
   )


constructStats :: Bool -> Session -> Html
constructStats isCorrect session =
   p ! [theclass "question"] <<
      (printf "Pass %d, question %d of %d total in this pass"
         pass passProbIx passTot :: String)
   +++
   p << (printf "%d (%0.1f%%) correct so far for this pass"
      correct perc :: String)

   where
      pass = sessPassNumber session
      passProbIx = sessPassProbIx session + 1
      passTot = sessPassTot session
      probIds = sessStudyList session

      corrModifier True  = 1
      corrModifier False = 0

      correct = passTot - (length probIds) + (corrModifier isCorrect)

      perc :: Float
      perc = (fromIntegral correct / fromIntegral passTot) * 100


lookupSqlValue ::
   (  Data.Convertible.Base.Convertible SqlValue a
   ,  Ord k )
   => k
   -> Map k SqlValue
   -> Maybe a
lookupSqlValue key mp = maybe Nothing fromSql $ lookup key mp


formSetup :: App CGIResult
formSetup = do
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

   setupPage <- liftIO $ do
      scriptPath <- getRelDataFileName "scripts/formSetup.js"
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
               -- This is the right-side content, it floats
               thespan ! [theclass "footer-right"] << (
                  anchor ! [theclass "footer-right", href "http://ui3.info/d/proj/grolprep"] << "developer site"
                  +++
                  (thespan ! [theclass "banner-dark-text"] << primHtml " &middot; ")
                  +++
                  feedback
                  )

               +++

               -- This is the left-side content, NOT floating
               "GROLPrep " +++
               thespan ! [theclass "banner-dark-text"] << appVersion
               )
            )
         )
   output $ renderHtml setupPage

   where
      constructSimOptionJs n =
         printf "         new Option('%s', '%s', false, false),"
            ((printf "Simulate Element %d exam" n) :: String)
            (show $ StudySimulation n)

      constructStudyOptionJs rsMap =
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
               , li ! [theclass "features"] << "Figure illustrations included!"
               ] )
         +++
         h3 << 
            (
            "This site provides a study tool and test simulator for the FCC GROL and Radar Endorsement examinations. GROLPrep is used by students of the " 
            +++ anchor ! [href "http://www.burlingtonaviationtech.org" ] 
               << "Avionics program at the Burlington Aviation Technology School"
            +++ "."
            )
         +++ 
         h3 << ("The source test questions can be acquired from the "
            +++ (anchor ! [href "http://wireless.fcc.gov/commoperators/index.htm?job=question_pools" ]  << "FCC Commercial Radio Operators License" )
            +++ " site.")


      theform rs13' rs8' =
         let study13     = "study13"
             study8      = "study8"
             studyCustom = "studyCustom"
         in thediv << (
         script << primHtml (
            "\n      var questionOpts13 = [\n" ++
            (unlines (map constructSimOptionJs [1, 3])) ++
            (unlines (map constructStudyOptionJs rs13')) ++ "      ];" ++

            "\n      var questionOpts8 = [\n" ++
            (constructSimOptionJs 8) ++
            (unlines (map constructStudyOptionJs rs8')) ++ "      ];"
         )
         +++
         form !
            [ method "POST"
            , action $ baseUrl ++ "/study/setup"
            ] <<
            fieldset <<
               [ legend <<
                  "Let's get started! Please select type of study"

               , thediv !
                  [ theclass "form-right"
                  , identifier "questions-parent"
                  ] <<
                  select !
                     [ identifier "questions"
                     , name "questions"
                     , size "12"
                     ]
                     << noHtml

               , thediv ! [theclass "form-left"] <<
                  [ ulist <<
                     [ li << label << (
                        radio "" study13 !
                           [ name "studyType"
                           , strAttr "id" study13
                           , checked
                           , strAttr "onclick" "populateQuestionsList()"
                           ]
                        +++ " GROL (Elements 1 and 3)" )
                     , li << label << (
                        radio "" study8 !
                           [ name "studyType"
                           , strAttr "id" study8
                           , strAttr "onclick" "populateQuestionsList()"
                           ]
                        +++ " Radar Endorsement (Element 8)" )
                     , li << label << (
                        radio "" studyCustom !
                           [ name "studyType"
                           , strAttr "id" studyCustom
                           , strAttr "onclick" "populateQuestionsList()"
                           ]
                        +++ " Enter a list of problem IDs" )
                     ]
                  , ulist <<
                     [ li << label ( checkbox "randQ" "" +++
                        " Ask the questions in a random order" )
                     , li << label ( checkbox "randA" "" +++
                        " Randomly order the answers of each question" )
                     ]
                  , submit (show ActStart) "Start study session"
                     ! [theclass "button"]
                  ]
               ]
         )


feedback :: Html
feedback = anchor !
   [ theclass "footer-right"
   , href $ baseUrl ++ "/feedback"
   ]
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


{- Produce the problem posing form
-}
formProblem :: App CGIResult
formProblem = do
   llog INFO "formProblem"

   session <- liftM fromJust getSession

   (mbnp, mbim) <- liftIO $ currentProblem session
   let (Problem pid q eas) = fromJust mbnp

   let aOrd = sessStudyAOrd session
   let nas = combineIxAndAns aOrd $ map extractAnswer eas

   im <- liftIO $ imgRegion mbim

   let fpp = formProblem' pid q nas im
   mi <- liftIO $ metaInfo pid
   posePage <- liftIO $ page [] $ formCancel +++ 
      mi +++ fpp +++ (constructStats False session)
   output $ renderHtml posePage

   where
      metaInfo pid = do
         ((_, elDesc), (seId, seDesc), (ktId, ktDesc))
            <- getProblemMetaInfo pid

         return $
            [ p << ((printf "%s" elDesc) :: String)
            , p << ((printf "Subelement %s: %s, Key Topic %d: %s"
                        seId seDesc ktId ktDesc) :: String)
            ]

      formProblem' pid q' as im' =
         form !
            [ theclass "question"
            , method "POST"
            , action $ baseUrl ++ "/study/problem"
            ] <<
            ( im' +++
            thediv <<
               [ p << (pid ++ ": " +++ (primHtml q'))
               , thediv << (ansControls as)
               , submit (show ActPose) "Proceed" ! [theclass "button"]
               ]
            )
         where
            ansControls as' = map f as'
               where
                  f :: (Int, String) -> Html
                  f (n', a) =
                     -- we pass empty string to radio so we can explictly set different name and id attributes 
                     p << ((radio "" (show n') ! [theclass "hanging", name "answer", strAttr "id" (show n')])
                          +++ label ! [thefor (show n')] << (primHtml a))


{- Compute whether or not the most recent answer was correct using
   data in the session
-}
isGuessCorrect :: Session -> App Bool
isGuessCorrect s = do
   let lastA = sessStudyLastA s
   let aOrd = sessStudyAOrd s
   (mbnp, _) <- liftIO $ currentProblem s
   let (Problem _ _ as) = fromJust mbnp
   let ast = combineIxAndAns aOrd as
   return $ either (const False) (const True) (snd $ ast !! lastA)


{- Produce the scoring answer form
-}
formAnswer :: App CGIResult
formAnswer = do
   session <- liftM fromJust getSession

   let g = sessStudyLastA session
   (mbnp, mbim) <- liftIO $ currentProblem session
   let pr@(Problem pid _ eas) = fromJust mbnp
   let aOrd = sessStudyAOrd session

   let nas = combineIxAndAns aOrd eas

   mi <- liftIO $ metaInfo pid
   im <- liftIO $ imgRegion mbim

   isCorrect <- isGuessCorrect session

   answerPage <- liftIO $ page [] $ formCancel +++
      mi +++ (theform g pr nas im) +++ (constructStats isCorrect session)
   output $ renderHtml answerPage

   where
      metaInfo pid = do
         ((_, elDesc), (seId, seDesc), (ktId, ktDesc))
            <- getProblemMetaInfo pid

         return $
            [ p << ((printf "%s" elDesc) :: String)
            , p << ((printf "Subelement %s: %s, Key Topic %d: %s"
                        seId seDesc ktId ktDesc) :: String)
            ]

      theform g (Problem pid q _) nas' im' =
         correctness (snd $ nas' !! g) +++
         form !
            [ theclass "question"
            , method "GET"
            , action $ baseUrl ++ "/study/next"
            ] << (
            im' +++
            thediv <<
               [ p << (pid ++ ": " +++ (primHtml q))
               , thediv << ansLines
               , submit (show ActAnswer) "Next question" ! [theclass "button"]
               ]
            )
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


{- Based on the order switching in the session, create a (possibly
   random) order for a set of answer indices, and store that in
   the session.
   If there's no session, will do nothing at all
-}
setAnswerOrder :: App ()
setAnswerOrder = do
   mbSession <- getSession
   maybe (return ())
      (\session -> do
         let randA = sessStudyRandA session

         aOrd <- liftIO $ orderer randA [0..3]
         putSession $ session { sessStudyAOrd = aOrd }
      ) mbSession


{- Action handlers
-}

initialize :: App CGIResult
initialize = do
   llog INFO "initialize"

   destroySession
   formSetup


setupSession :: App CGIResult
setupSession = do
   llog INFO "setupSession"

   studyType <- getInput "studyType"
   mbQuestionsChoice <- case studyType of
      Just "studyCustom" ->
         liftM (Just . StudyCustom . fromJust) $ getInput "questions"
      Just _ -> readInput "questions"
      Nothing -> return Nothing  -- This should never happen

   case mbQuestionsChoice of
      Just (StudySimulation element) -> do
         problems <- liftIO $ getSimProblemIds element
         finishSetup problems

      Just (StudyRegular element subelement) -> do
         problems <- liftIO $ getRegularProblemIds element subelement
         finishSetup problems

      Just (StudyCustom problemsString) -> do
         let problems = wordsBy (== ' ') $ map toUpper problemsString
         finishSetup problems

      -- No questions were selected, loop back to the beginning
      Nothing -> initialize

   where
      finishSetup problems = do
         randA <- liftM (maybe False (const True)) $ getInput "randA"

         questionOrderer <- liftM (maybe return (const shuffle))
            $ getInput "randQ"

         sortedProblems <- liftIO $ questionOrderer problems

         putSession $ Session
               { sessPassNumber  = 1
               , sessPassTot     = length sortedProblems
               , sessPassProbIx  = 0

               , sessStudyRandA  = randA
               , sessStudyList   = sortedProblems
               , sessStudyProbIx = 0
               , sessStudyAOrd   = []
               , sessStudyLastA  = 0  -- This is a dummy value
               }
         setAnswerOrder
         formProblem


prepareForNext :: App CGIResult
prepareForNext = do
   llog INFO "prepareForNext"

   oldSession <- liftM fromJust getSession
   let passProbIx = sessPassProbIx oldSession
   let oldList = sessStudyList oldSession
   let probIx = sessStudyProbIx oldSession

   isCorrect <- isGuessCorrect oldSession
   let (newProbIx, newList) = case isCorrect of
         True  -> (probIx, removeFromList probIx oldList)
         False -> (probIx + 1, oldList)

   putSession $ oldSession
      { sessPassProbIx = passProbIx + 1
      , sessStudyList = newList
      , sessStudyProbIx = newProbIx
      }

   session <- liftM fromJust getSession
   let list = sessStudyList session
   (mbnp, _) <- liftIO $ currentProblem session

   case (mbnp, null list) of
      -- We have a next problem, let's get to it
      (Just _, _    ) -> do
         setAnswerOrder
         formProblem

      -- No next problem and there are no more
      -- not-correctly-answered. We're done.
      (_      , True ) -> initialize

      -- No next problem for this pass, but not-correctly-answered
      -- problems remain. Start next pass.
      (_      , False) -> do
         let pass = sessPassNumber session
         putSession $ session
               { sessPassNumber = pass + 1
               , sessPassProbIx = 0
               , sessPassTot = (length list)
               , sessStudyProbIx = 0
               }
         setAnswerOrder
         formProblem


evalProblem :: App CGIResult
evalProblem = do
   llog INFO "evalProblem"

   -- Get session and extract some things from it
   mbSession <- getSession

   -- Pull the answer they selected out of the form
   mbAnswer <- readInput "answer"

   case (mbSession, mbAnswer) of
      -- We have a session and the user answered
      (Just session, Just answer) -> do
         putSession $ session { sessStudyLastA = answer }
         formAnswer

      -- We have no session and yet they somehow arrived at this form
      -- Get out of here and go back to the beginning
      --(Nothing, _) -> actionInitialize
      (Nothing, _) -> initialize

      -- The user has submit the form with no answer selected
      -- Kick them right back to the pose problem form
      (Just _, _) -> formProblem
