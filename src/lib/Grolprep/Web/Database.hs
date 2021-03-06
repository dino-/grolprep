-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

module Grolprep.Web.Database
   ( getSimProblemIds, getRegularProblemIds
   , currentProblem
   , getProblemMetaInfo
   , constructSetupOptions
   )
   where

import Control.Monad
import Data.Convertible.Base
import Data.List ( intercalate )
import Data.Map ( Map, lookup )
import Data.Maybe
import Database.HDBC
import Database.HDBC.Sqlite3
import Prelude hiding ( lookup )
import System.Random
import Text.Printf
import Text.XHtml.Strict

import Grolprep.Common.Data
import Grolprep.Common.Util ( getDataFilePath )
import Grolprep.Web.Session


dbPath :: IO FilePath
dbPath = getDataFilePath $ "grolprep.sqlite"


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

   _ <- execute stmt [toSql element, toSql subelement]
   rs <- sFetchAllRows' stmt
   disconnect conn
   return $ map fromJust $ concat rs


{- Using the supplied session, get the current Problem and associated
   image (if any)
-}
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
         _ <- execute stmt [toSql $ probIds !! probIx]
         rs <- liftM concat $ sFetchAllRows' stmt
         disconnect conn

         let mbProblem = maybe Nothing (Just . read) $ head rs
         return (mbProblem, last rs)
      else return (Nothing, Nothing)


{- Convenience function to lookup a key in some SQL query results
   and extract it with fromSql
-}
lookupSqlValue ::
   (  Data.Convertible.Base.Convertible SqlValue a
   ,  Ord k )
   => k
   -> Map k SqlValue
   -> Maybe a
lookupSqlValue key mp = maybe Nothing fromSql $ lookup key mp


{- Given a problem ID, get all the identifying info and text descriptions
   associated with it from the db
-}
getProblemMetaInfo :: String -> 
   IO ((Int, String), (String, String), (Int, String))
getProblemMetaInfo problemId = do
   conn <- dbPath >>= connectSqlite3

   (pEl:pSe:pKt:elDesc:seDesc:ktDesc:_) <- liftM concat $
      quickQuery' conn ( unlines
         [ "SELECT"
         , "   p.element, p.subelement, p.keytopic, "
         , "   el.desc, se.desc, kt.desc"
         , "   FROM problem p"
         , "   LEFT JOIN element el"
         , "      ON p.element = el.id"
         , "   LEFT JOIN subelement se"
         , "      ON p.subelement = se.id"
         , "      AND p.element = se.element"
         , "   LEFT JOIN keytopic kt"
         , "      ON p.keytopic = kt.id"
         , "      AND p.element = kt.element"
         , "      AND p.subelement = kt.subelement"
         , "   WHERE p.id = ?"
         , ";"
         ] )
         [toSql problemId]

   disconnect conn

   return
      ( (fromSql pEl, fromSql elDesc)
      , (fromSql pSe, fromSql seDesc)
      , (fromSql pKt, fromSql ktDesc)
      )


{- Given a list of elements, construct option form elements for the
   subelements in them. This is for the setup form
-}
constructSetupOptions :: [Int] -> IO [Html]
constructSetupOptions es = do
   conn <- dbPath >>= connectSqlite3

   let inExpr = "(" ++ (intercalate "," $ map show es) ++ ")"

   stmt <- prepare conn $ unlines
      [ "SELECT id, element, desc FROM subelement "
      , "   WHERE element IN " ++ inExpr
      , "   ORDER BY element, id"
      , ";"
      ]
   _ <- execute stmt []
   rsMap <- fetchAllRowsMap' stmt

   return $ 
      (map constructSimOption es)
      ++
      (map constructStudyOption rsMap)

   where
      constructSimOption n =
         option ! [value (show $ StudySimulation n)]
            << ((printf "Simulate Element %d exam" n) :: String)

      constructStudyOption rsMap =
         option ! [value (show $ StudyRegular elValue seValue)] << seDesc
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
