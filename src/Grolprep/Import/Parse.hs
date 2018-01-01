-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Grolprep.Import.Parse
   ( Element (..), SubElement (..), KeyTopic (..)
   , parseProblems
   )
   where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
--import Debug.Trace
import Text.ParserCombinators.Parsec

import Grolprep.Common.Data


data Element = Element Int String [SubElement]
   deriving Show

data SubElement = SubElement String String [KeyTopic]
   deriving Show

data KeyTopic = KeyTopic Int String [Problem]
   deriving Show


assignCorrectAns :: Int -> [Answer] -> [Answer]
assignCorrectAns c as = map (g c) $ zip [0..] as
   where
      g c' (n, Left a)
         | c' == n   = Right a
         | otherwise = Left a
      g _ (_, Right _) = undefined


correctAnswers :: [Problem] -> [(ProblemId, Char)] -> [Problem]
correctAnswers ps ss = map f ps
   where
      f (Problem pid q as) = Problem pid q $ assignCorrectAns (c pid) as

      c pid' = letToNum $ fromJust $ lookup pid' ss

      letToNum 'A' = 0
      letToNum 'B' = 1
      letToNum 'C' = 2
      letToNum 'D' = 3
      letToNum _   = undefined


{- This function is useful for debugging
pTrace :: String -> GenParser Char st ()
pTrace s = pt <|> return ()
   where
      pt = try $ do
         x <- try $ many1 anyChar
         --trace (s ++ ": " ++ x) $ try $ char 'z'
         trace (s ++ ": " ++ (take 255 x)) $ try $ char 'z'
         fail x
-}

eol :: GenParser Char st Char
eol = newline <|> (eof >> return '\n')

tillEol :: GenParser Char st String
tillEol = manyTill (noneOf "\n") eol


ansLetter :: CharParser st Char
ansLetter = oneOf "ABCD"


problemId :: GenParser Char st ProblemId
problemId = do
   ele <- digit
   hyph <- char '-'
   keyt <- many1 digit
   subel <- letter
   qnum <- many1 digit
   return $ ele : hyph : keyt ++ [subel] ++ qnum


question :: GenParser Char st (ProblemId, Question)
question = do
   pid <- problemId <?> "problemId"
   spaces
   t <- tillEol <?> "{question text}"
   return (pid, t)


answer :: GenParser Char st Answer
answer = do
   spaces
   ansLetter
   char '.'
   spaces
   liftM Left tillEol


problem :: GenParser Char st Problem
problem = do
   spaces
   (pid, q) <- question <?> "question"
   as <- (many1 $ try answer) <?> "answer"

   return $ Problem pid q as


solutions :: GenParser Char st [(ProblemId, Char)]
solutions = do
   spaces
   string "Answer Key:"
   ss <- many1 $ try $ do
      spaces
      pid <- problemId
      char ':' >> spaces
      s <- ansLetter
      return (pid, s)
   return ss


keyTopicHead :: GenParser Char st (String, String)
keyTopicHead = do
   spaces
   string "Key Topic "
   ktNum <- many1 digit
   char ':'
   spaces
   ktDesc <- tillEol
   return (ktNum, ktDesc)


keyTopic :: GenParser Char st KeyTopic
keyTopic = do
   (n, desc) <- try keyTopicHead <?> "keyTopicHead"
   ps <- (many1 $ try problem) <?> "problem"
   ss <- solutions <?> "solutions"

   let finalProblems = correctAnswers ps ss

   return $ KeyTopic (read n) desc finalProblems


subElementHead :: GenParser Char st (Char, String)
subElementHead = do
   spaces
   string "Subelement"
   spaces
   seLetter <- letter
   spaces >> char '-' >> spaces
   seDesc <- manyTill anyChar $ char ':'
   tillEol
   return (seLetter, seDesc)


subElement :: GenParser Char st SubElement
subElement = do
   (l, desc) <- try subElementHead <?> "subElementHead"
   kts <- (many1 $ try keyTopic) <?> "keyTopic"

   return $ SubElement [l] desc kts


elemDesc :: GenParser Char st (Int, String)
elemDesc = do
   spaces
   y <- many1 $ satisfy (not . isDigit)
   d <- many1 digit
   z <- manyTill anyChar $ try $ string " ("
   tillEol
   return (read d, (y ++ d ++ z))


element :: GenParser Char st Element
element = do
   (en, ed) <- elemDesc
   ses <- (many1 $ try subElement) <?> "subElement"
   spaces
   string "[END OF"
   tillEol

   return $ Element en ed ses


parseProblems :: String -> Either ParseError Element
parseProblems = parse element "grolprep study data parse"
