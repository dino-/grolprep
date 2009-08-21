-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>


module Fequiz.Session
   ( StudyType (..), Session (..)
   )
   where


data StudyType
   = Set String  -- A specific question set, this is the path
                 -- like "resources/3a.txt"
   -- | Sim         -- A simulation of a real 76 question test
   deriving (Read, Show)


data Session = Session
   { sessType :: StudyType
   , sessSeed :: Maybe Int  -- Nothing means no randomization
   , sessCurr :: Int
   , sessResults :: Integer  -- Results for all questions in a bit field
   }
   deriving (Read, Show)
