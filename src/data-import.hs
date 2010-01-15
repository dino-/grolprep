#! /usr/bin/runhaskell -isrc

-- Copyright: 2009, 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory
import System.Environment
import System.FilePath
import System.IO

import Grolprep.Common.Data
import Grolprep.Import.Parse


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
         ,  [ "CREATE TABLE figure "
            , "   ( id TEXT PRIMARY KEY"
            , "   , figure TEXT"
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


storeFigures conn = do
   stmt <- prepare conn $ unlines
      [ "INSERT INTO figure "
      , "   (id, figure)"
      , "   VALUES (?, ?)"
      , ";"
      ]

   executeMany stmt figureMappings
   commit conn

   where figureMappings =
            [ [ toSql "3-10B4", toSql "3B1"]
            , [ toSql "3-10B5", toSql "3B2"]
            , [ toSql "3-12B6", toSql "3B3"]
            , [ toSql "3-20C2", toSql "3C4"]
            , [ toSql "3-20C3", toSql "3C4"]
            , [ toSql "3-20C4", toSql "3C5"]
            , [ toSql "3-20C5", toSql "3C5"]
            , [ toSql "3-30D6", toSql "3D6"]
            , [ toSql "3-32D1", toSql "3D7"]
            , [ toSql "3-32D2", toSql "3D8"]
            , [ toSql "3-32D4", toSql "3D9"]
            , [ toSql "3-32D5", toSql "3D10"]
            , [ toSql "3-32D6", toSql "3D11"]
            , [ toSql "3-35E2", toSql "3E12"]
            , [ toSql "3-35E3", toSql "3E13"]
            , [ toSql "3-35E5", toSql "3E14"]
            , [ toSql "3-43F6", toSql "3F15"]
            , [ toSql "3-47F2", toSql "3F16"]
            , [ toSql "3-50F5", toSql "3F15"]
            , [ toSql "3-58H1", toSql "3H17"]
            , [ toSql "3-58H2", toSql "3H17"]
            , [ toSql "8-7A1", toSql "8A1"]
            , [ toSql "8-7A6", toSql "8A1"]
            , [ toSql "8-9A2", toSql "8A2"]
            , [ toSql "8-9A4", toSql "8A2"]
            , [ toSql "8-9A5", toSql "8A3"]
            , [ toSql "8-10A1", toSql "8A4"]
            , [ toSql "8-10A2", toSql "8A5"]
            , [ toSql "8-10A3", toSql "8A6"]
            , [ toSql "8-10A4", toSql "8A7"]
            , [ toSql "8-10A5", toSql "8A2"]
            , [ toSql "8-10A6", toSql "8A8"]
            , [ toSql "8-20C1", toSql "8C9"]
            , [ toSql "8-20C5", toSql "8A1"]
            , [ toSql "8-21C3", toSql "8A1"]
            , [ toSql "8-24C6", toSql "8A1"]
            , [ toSql "8-26C3", toSql "8C10"]
            , [ toSql "8-26C4", toSql "8C11"]
            , [ toSql "8-26C6", toSql "8C11"]
            , [ toSql "8-45F1", toSql "8F12"]
            , [ toSql "8-45F4", toSql "8F12"]
            ]


maybeHead [] = Nothing
maybeHead l  = Just $ head l


dbSetup = do
   let dbPath = "resources" </> "grolprep" <.> "sqlite"
   dbExists <- doesFileExist dbPath

   conn <- connectSqlite3 dbPath

   unless dbExists $ createTables conn

   return conn


main :: IO ()
main = do
   arg <- liftM maybeHead getArgs

   case arg of
      Just "figures" -> do
         conn <- dbSetup

         storeFigures conn

         disconnect conn

      Just dataPath -> do
         conn <- dbSetup

         eel <- liftM parseProblems $ readFile dataPath
         let el = either undefined id eel
         storeElement conn el

         disconnect conn

      Nothing -> do
         putStrLn "ERROR bad arguments"
