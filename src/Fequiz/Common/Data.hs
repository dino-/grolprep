-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>


module Fequiz.Common.Data
   ( ProblemId, Question, Answer, Problem (..)
   , extractAnswer
   )
   where


type ProblemId = String

type Question = String

type Answer = Either String String

data Problem = Problem ProblemId Question [Answer]
   deriving Show


extractAnswer :: Either a a -> a
extractAnswer = either id id
