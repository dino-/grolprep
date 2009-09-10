#! /usr/bin/runhaskell -isrc

-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory
import System.Environment
import System.FilePath
import System.IO

import Fequiz.Common.Data
import Fequiz.Import.Parse


{- Create the necessary tables
-}
createTables conn = do
   mapM_ (\sql -> do
         stmt <- prepare conn sql
         execute stmt []
      ) creationSql

   commit conn

   where
      creationSql = map unlines
         [  [ "CREATE TABLE element "
            , "   ( id   INTEGER PRIMARY KEY"
            , "   , desc TEXT"
            , "   )"
            , ";"
            ]
         ,  [ "CREATE TABLE subelement "
            , "   ( id   TEXT"
            , "   , element INTEGER"
            , "   , desc TEXT"
            , "   )"
            , ";"
            ]
         ,  [ "CREATE TABLE keytopic "
            , "   ( id   INTEGER"
            , "   , element INTEGER"
            , "   , subelement TEXT"
            , "   , desc TEXT"
            , "   )"
            , ";"
            ]
         ,  [ "CREATE TABLE problem "
            , "   ( id           TEXT PRIMARY KEY"
            , "   , element      INTEGER"
            , "   , subelement   TEXT"
            , "   , keytopic     INTEGER"
            , "   , probdata     TEXT"
            , "   )"
            , ";"
            ]
         ]


storeElement conn (Element eid edesc ses) = do
   stmt <- prepare conn $ unlines
      [ "INSERT INTO element "
      , "   (id, desc)"
      , "   VALUES (?, ?)"
      , ";"
      ]

   execute stmt [toSql eid, toSql edesc]
   commit conn

   mapM_ (storeSubElement conn eid) ses


storeSubElement conn eid (SubElement seid sedesc kts) = do
   stmt <- prepare conn $ unlines
      [ "INSERT INTO subelement "
      , "   (id, element, desc)"
      , "   VALUES (?, ?, ?)"
      , ";"
      ]

   execute stmt [toSql seid, toSql eid, toSql sedesc]
   commit conn

   mapM_ (storeKeyTopic conn eid seid) kts


storeKeyTopic conn eid seid (KeyTopic kid kdesc ps) = do
   stmt <- prepare conn $ unlines
      [ "INSERT INTO keytopic "
      , "   (id, element, subelement, desc)"
      , "   VALUES (?, ?, ?, ?)"
      , ";"
      ]

   execute stmt [toSql kid, toSql eid, toSql seid, toSql kdesc]
   commit conn

   mapM_ (storeProblem conn eid seid kid) ps


storeProblem conn eid seid kid p@(Problem pid _ _) = do
   stmt <- prepare conn $ unlines
      [ "INSERT INTO problem "
      , "   (id, element, subelement, keytopic, probdata)"
      , "   VALUES (?, ?, ?, ?, ?)"
      , ";"
      ]

   execute stmt
      [ toSql pid
      , toSql eid
      , toSql seid
      , toSql kid
      , toSql $ show p
      ]
   commit conn


main :: IO ()
main = do
   let dbPath = "resources" </> "fequiz" <.> "sqlite"

   dbExists <- doesFileExist dbPath

   conn <- connectSqlite3 dbPath

   unless dbExists $ createTables conn

   dataPath <- liftM head getArgs
   eel <- liftM parseProblems $ readFile dataPath
   let el = either undefined id eel
   storeElement conn el

   disconnect conn
