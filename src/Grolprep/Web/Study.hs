-- Copyright: 2009, 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>


module Grolprep.Web.Study
   where

import Control.Monad
import Data.Char
import Data.List hiding ( lookup )
import Data.List.Split
import Data.Maybe
import Network.CGI
import Prelude hiding ( lookup )
import System.FilePath
import Text.Printf
import Text.XHtml.Strict

import Grolprep.Common.Data
import Grolprep.Common.Log
import Grolprep.Common.Util
import Grolprep.Common.Shuffle
import Grolprep.Web.Database
import Grolprep.Web.Session
import Grolprep.Web.Util


{- Remove an indexed element from a list
-}
removeFromList :: Int -> [a] -> [a]
removeFromList i xs = take i xs ++ drop (i + 1) xs


imgRegion :: Maybe FilePath -> IO Html
imgRegion Nothing = return noHtml
imgRegion (Just im) = do
   path <- getRelDataFileName $ "figures" </> im <.> "png"
   return $ image ! [theclass "problem", src path]


{- This function is used to take a list of randomized indexes to
   answers, and a list of the answers themselves, combine them into
   tuples, and sort that new list on the index values.
-}
combineIxAndAns :: (Ord a) => [a] -> [b] -> [(a, b)]
combineIxAndAns xs ys =
   sortBy (\x y -> compare (fst x) (fst y)) $ zip xs ys


{- Used to map a Bool value to a shuffle/don't-shuffle function
-}
orderer :: Bool -> [a] -> IO [a]
orderer True  = shuffle
orderer False = return


{- HTML pages and forms 
-}

formCancel :: String -> Html
formCancel burl = form !
   [ method "get"
   , action $ burl ++ "/init"
   ] << (
   submit "quit" "Cancel test session" ! [theclass "button"]
   )


{- Builds the section of xhtml that describes current study progress
-}
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


{- Identifiers we need in several places
-}
nameRadioGroup, nameRadioStudy13, nameSelectStudy13, nameRadioStudy8, nameSelectStudy8, nameRadioStudyCustom, nameTextareaStudyCustom :: String
nameRadioGroup          = "studyType"
nameRadioStudy13        = "radio-study13"
nameSelectStudy13       = "select-study13"
nameRadioStudy8         = "radio-study8"
nameSelectStudy8        = "select-study8"
nameRadioStudyCustom    = "radio-studycustom"
nameTextareaStudyCustom = "select-studycustom"


{- Construct the setup form
-}
formSetup :: App CGIResult
formSetup = do
   burl <- liftIO baseUrl

   scriptPath <- liftIO $ getRelDataFileName "scripts/formSetup.js"

   opts13 <- liftIO $ constructSetupOptions [1, 3]
   opts8 <- liftIO $ constructSetupOptions [8]

   setupPage <- liftIO $ do
      cssLinks <- createCssLinks [ "css/setup.css" ]
      return (
         ( header <<
            ( titleBar +++
              cssLinks +++
              ( script !
                  [ src scriptPath
                  , thetype "text/javascript"
                  ] << noHtml
              )
            )
         )
         +++
         body ! [strAttr "onload" "setInitialSelections()"] <<
            ([heading, about, theform opts13 opts8 burl] 
             +++
             thediv ! [theclass "banner"] << h2 ! [theclass "footer"] << (
               -- This is the right-side content, it floats
               thespan ! [theclass "footer-right"] << (
                  anchor ! [theclass "footer-right", href "http://ui3.info/d/proj/grolprep"] << "developer site"
                  +++
                  (thespan ! [theclass "banner-dark-text"] << primHtml " &middot; ")
                  +++
                  feedback burl
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


      theform opts13' opts8' burl' = thediv << (
         form !
            [ method "post"
            , action $ burl' ++ "/study/setup"
            ] <<
            fieldset <<
               [ legend <<
                  "Let's get started! Please select type of study"

               , thediv <<
                  [ select !
                     [ identifier nameSelectStudy13
                     , name nameSelectStudy13
                     , size "5"
                     , strAttr "onclick" "setRadio('study13')"
                     , intAttr "tabindex" 2
                     ]
                     << opts13'
                  , label ! [thefor nameRadioStudy13] << (
                     radio "" nameRadioStudy13 !
                        [ identifier nameRadioStudy13
                        , name nameRadioGroup
                        , intAttr "tabindex" 1
                        , checked
                        ]
                     +++ " GROL (Elements 1 and 3)" )
                  ]

               , thediv <<
                  [ select !
                     [ identifier nameSelectStudy8
                     , name nameSelectStudy8
                     , size "5"
                     , strAttr "onclick" "setRadio('study8')"
                     , intAttr "tabindex" 3 
                     ]
                     << opts8'
                  , label ! [thefor nameRadioStudy8] << (
                     radio "" nameRadioStudy8 !
                        [ identifier nameRadioStudy8
                        , name nameRadioGroup
                        ]
                     +++ " Radar Endorsement (Element 8)" )
                  ]

               , thediv <<
                  [ textarea !
                     [ name nameTextareaStudyCustom
                     , rows "3"
                     , strAttr "onclick" "setRadio('studycustom')"
                     , intAttr "tabindex" 4 
                     ]
                     << "1-11B4 3-1A5 3-11B1 3-17B2 3-22C1 3-35E5 3-38E1 3-90O2 3-96P2 8-7A1"
                  , label ! [thefor nameRadioStudyCustom] << (
                     radio "" nameRadioStudyCustom !
                        [ identifier nameRadioStudyCustom
                        , name nameRadioGroup
                        ]
                     +++ " Enter a list of problem IDs" )
                  ]

               , thediv <<
                  [ label ( checkbox "randQ" "" ! [ intAttr "tabindex" 5 ] +++
                     " Ask the questions in a random order" )
                  , label ( checkbox "randA" "" ! [ intAttr "tabindex" 6 ] +++
                     " Randomly order the answers of each question" )
                  ]

               , submit "start" "Start study session" ! 
                  [ theclass "button"
                  , intAttr "tabindex" 7
                  ]
               ]
            )


{- Construct the xhtml for the feedback link on the setup form
-}
feedback :: String -> Html
feedback burl = anchor !
   [ theclass "footer-right"
   , href $ burl ++ "/feedback"
   ]
   << "feedback"


{- Produce the problem posing form
-}
formProblem :: App CGIResult
formProblem = do
   llog INFO "formProblem"

   session <- liftM fromJust getSession

   burl <- liftIO baseUrl

   (mbnp, mbim) <- liftIO $ currentProblem session
   let (Problem pid q eas) = fromJust mbnp

   let aOrd = sessStudyAOrd session
   let nas = combineIxAndAns aOrd $ map extractAnswer eas

   im <- liftIO $ imgRegion mbim

   let fpp = formProblem' pid q nas im burl
   mi <- liftIO $ metaInfo pid
   posePage <- liftIO $ page [] $ (formCancel burl) +++ 
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

      formProblem' pid q' as im' burl' =
         form !
            [ theclass "question"
            , method "post"
            , action $ burl' ++ "/study/problem"
            ] <<
            ( im' +++
            thediv <<
               [ p << (pid ++ ": " +++ (primHtml q'))
               , thediv << (ansControls as)
               , submit "submit" "Submit answer" ! [theclass "button"]
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
   llog INFO "formAnswer"

   session <- liftM fromJust getSession

   burl <- liftIO baseUrl

   let g = sessStudyLastA session
   (mbnp, mbim) <- liftIO $ currentProblem session
   let pr@(Problem pid _ eas) = fromJust mbnp
   let aOrd = sessStudyAOrd session

   let nas = combineIxAndAns aOrd eas

   mi <- liftIO $ metaInfo pid
   im <- liftIO $ imgRegion mbim

   isCorrect <- isGuessCorrect session

   answerPage <- liftIO $ page [] $ (formCancel burl) +++
      mi +++ (theform g pr nas im burl) +++ (constructStats isCorrect session)
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

      theform g (Problem pid q _) nas' im' burl' =
         correctness (snd $ nas' !! g) +++
         form !
            [ theclass "question"
            , method "get"
            , action $ burl' ++ "/study/next"
            ] << (
            im' +++
            thediv <<
               [ p << (pid ++ ": " +++ (primHtml q))
               , thediv << ansLines
               , submit "continue" "Continue" ! [theclass "button"]
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


{- Initialize the web application. This means destroy the session, 
   if any, and display the setup form
-}
initialize :: App CGIResult
initialize = do
   llog INFO "initialize"

   destroySession
   formSetup


{- Handle the submitted setup form
-}
setupSession :: App CGIResult
setupSession = do
   llog INFO "setupSession"

   mbQuestionsChoice <- getInput nameRadioGroup >>= getQuestionsChoice

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
      getQuestionsChoice (Just ty)
         | ty == nameRadioStudy13 = readInput nameSelectStudy13
         | ty == nameRadioStudy8 = readInput nameSelectStudy8
         | ty == nameRadioStudyCustom =
            liftM (Just . StudyCustom . fromJust) $
               getInput nameTextareaStudyCustom
         | otherwise = return Nothing
      getQuestionsChoice Nothing = return Nothing

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


{- Adjust the state to ready us for the next problem. This function
   also deals with situations like moving to the next pass and
   detecting when we're finished
-}
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


{- Handle the submitted problem form
-}
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
      (Nothing, _) -> initialize

      -- The user has submit the form with no answer selected
      -- Kick them right back to the pose problem form
      (Just _, _) -> formProblem


{- Right after the answer form comes back, check to see if we need
   to show pass summary before moving on to the next problem
-}
evalPass :: App CGIResult
evalPass = do
   llog INFO "evalPass"

   session <- liftM fromJust getSession

   let passTot = sessPassTot session
   let passProbIx = sessPassProbIx session + 1

   case (passProbIx == passTot) of
      True  -> formPassSummary
      False -> prepareForNext


{- Produce the pass summary form
-}
formPassSummary :: App CGIResult
formPassSummary = do
   llog INFO "formPassSummary"

   session <- liftM fromJust getSession

   burl <- liftIO baseUrl

   isCorrect <- isGuessCorrect session
   let pass = sessPassNumber session
   let passTot = sessPassTot session
   let probIds = sessStudyList session
   let correct = passTot - (length probIds) + (corrModifier isCorrect)
   let perc = ( (fromIntegral correct / fromIntegral passTot) * 100 )
         :: Float

   passSummaryPage <- liftIO $ page [] $
      (formCancel burl) +++
      h2 << (printf "Pass %d completed" pass :: String) +++
      ( h3 << (printf "%d of %d answered correctly (%0.1f%%)"
         correct passTot perc :: String) ) +++
      theform (passTot - correct) burl
   output $ renderHtml passSummaryPage

   where
      corrModifier True  = 1
      corrModifier False = 0

      buttonLabel 0 = "Finish"
      buttonLabel _ = "Continue"

      theform remaining burl' =
         form !
            [ theclass "question"
            , method "post"
            , action $ burl' ++ "/study/psummary"
            ] << (
            thediv <<
               [ submit "next" (buttonLabel remaining) ! [theclass "button"]
               ]
            )
