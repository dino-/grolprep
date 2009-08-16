-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad
import Network.CGI
import Text.XHtml.Strict

import Fequiz.Data


problemForm (Problem n q eas) = form << (
   [ paragraph << ((show n) ++ "] " ++ q)
   ] ++ (map ans eas) ++
   [ submit "" "Proceed"
   ] )
   where
      ans ea = paragraph << (extractAnswer ea)


page t b = header << thetitle << t +++ body << b


cgiMain problem = do
   output $ renderHtml $ page "foo" $ problemForm problem


main :: IO ()
main = do
   eps <- liftM parseProblems $ readFile "resources/1.txt"

   --either print (\(_,ps) -> print $ head ps) eps
   --either print (\(_,ps) -> print $ take 2 ps) eps

   let problem = either undefined (\(_,ps) -> head ps) eps
   runCGI $ handleErrors $ cgiMain problem
