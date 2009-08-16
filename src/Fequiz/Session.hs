-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>


module Fequiz.Session
   where


data StudyType
   = Set String  -- A specific question set, this is the path
                 -- like "resources/3a.txt"
   | Sim Int     -- A simulation of a real 76 question test
                 -- the Int is the random number seed so it can be
                 -- recreated
   deriving (Read, Show)


data Session = Session
   { sessType = StudyType
   , sessCurr = Int
   , sessResults = [(Int, Bool)]
   }
   deriving (Read, Show)
