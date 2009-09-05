-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>


module Fequiz.Common.Data
   ( Number, Question, Answer, Problem (..)
   , parseProblems
   , extractAnswer
   )
   where

import Control.Monad
import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.Regex


type Number = Int

type Question = String

type Answer = Either String String

data Problem = Problem Number Question [Answer]
   deriving Show


{- Convert a list of answer strings into a list of Either String String
   where the Right element is the one marked with the @@ string
-}
convertAnswers :: [String] -> [Answer]
convertAnswers = map convert
   where
      convert s = maybe (Left s) (Right . head) (match s)
      match = matchRegex $ mkRegex "(.*)@@"


extractAnswer :: Either a a -> a
extractAnswer = either id id


{- The code in this section combines multiline continued questions and 
   answers into single lines. This is a preprocessing step that makes 
   further parsing easier.
-}

-- Combine the lines
deMultiLine :: [String] -> [String]
deMultiLine = reverse . (foldl' f [])
   where f st l = (fromJust $ foldr mplus Nothing (matchActions l)) st

{- Create the list of Maybe actions which will be tried until one
   fits the pattern in the current line
-}
matchActions :: String -> [Maybe ([String] -> [String])]
matchActions l =
   map ($ l) [ matchJunk, matchQStart, matchAStart, matchCont ]

-- Matches a blank line, do nothing to the state
matchJunk :: String -> Maybe (a -> a)
matchJunk l = do
   matchRegex (mkRegex "^\\s*$") l
   return id

-- Matches a question start line
matchQStart :: String -> Maybe ([String] -> [String])
matchQStart l = do
   matchRegex (mkRegex "^[0-9]{1,4}\\]") l
   return (l :)

-- Matches an answer start line
matchAStart :: String -> Maybe ([String] -> [String])
matchAStart l = do
   matchRegex (mkRegex "^[a-d]\\}") l
   return (l :)

-- Everything else, append to the prior line
-- This one always succeeds, use it LAST
matchCont :: String -> Maybe ([String] -> [String])
matchCont l = return h
   where
      h (m:ms) = (m ++ " " ++ l) : ms
      h []     = [l]


{- The code in this section parses the lines into a list of data of 
   type Problem using Parsec
-}

eol :: GenParser Char st Char
eol = newline <|> (eof >> return '\n')

tillEol :: GenParser Char st String
tillEol = manyTill (noneOf "\n") eol


parseProblems :: String -> Either ParseError (String, [Problem])
parseProblems =
   (parse problems "fequiz FCC study data parse") .
   unlines .
   deMultiLine .
   lines

   where
      problems = do
         h <- try (manyTill (noneOf "]\n") eol) <|> (return "unnamed")
         ps <- many1 problem
         return (h, ps)

      problem = do
         (n, t) <- question <?> "question"
         as <- many1 answer <?> "answers"
         return $ Problem (read n) t $ convertAnswers as

      question = do
         n <- many1 digit
         char ']'
         spaces
         t <- tillEol
         return (n, t)

      answer = do
         oneOf "abcd"
         char '}'
         spaces
         tillEol
