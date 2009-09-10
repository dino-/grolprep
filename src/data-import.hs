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
import Text.Printf

import Fequiz.Import.Parse


{- Remove a file given a file path. Does nothing at all if the file
   does not exist.
-}
unlink :: FilePath -> IO ()
unlink path = doesFileExist path >>= (flip when $ removeFile path)


dbPath :: FilePath
dbPath = "resources" </> "fequiz" <.> "sqlite"


{- Create the necessary tables
-}
createTable conn = do
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
            , "   ( id   TEXT PRIMARY KEY"
            , "   , desc TEXT"
            , "   )"
            , ";"
            ]
         ,  [ "CREATE TABLE keytopic "
            , "   ( id   INTEGER PRIMARY KEY"
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


main :: IO ()
main = do
   unlink dbPath
   conn <- connectSqlite3 dbPath

   createTable conn

{-
   dataPath <- liftM head getArgs
   eps <- liftM parseProblems $ readFile dataPath
   print eps
-}

   disconnect conn
