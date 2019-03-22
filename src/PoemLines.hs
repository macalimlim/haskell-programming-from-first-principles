{-# LANGUAGE NoImplicitPrelude #-}

module PoemLines where

import Chapter9      (breaker)
import Data.Bool     (Bool (False, True))
import Data.Char     (Char)
import Data.Eq       ((==))
import Data.Function (($))
import Data.List     (dropWhile, takeWhile, (++))
import Data.String   (String)
import System.IO     (IO, print)
import Text.Show     (show)

firstSen :: String
firstSen = "Tyger Tyger, burning bright\n"

secondSen :: String
secondSen = "In the forests of the night\n"

thirdSen :: String
thirdSen = "What immortal hand or eye\n"

fourthSen :: String
fourthSen = "Could frame thy fearful symmetry?"

sentences :: String
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

isNotNewLine :: Char -> Bool
isNotNewLine '\n' = False
isNotNewLine _    = True

-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?
-- Implement this
myLines :: String -> [String]
myLines ""          = []
myLines ('\n' : xs) = myLines xs
myLines xs          = takeWhile isNotNewLine xs :
                        myLines (dropWhile isNotNewLine xs)

-- What we want 'myLines sentences' to equal
shouldEqual :: [String]
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

-- The main function here is a small test
-- to ensure you've written your function
-- correctly.
main :: IO ()
main = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)

-- from Chapter 9 exercise
myLines' :: String -> [String]
myLines' = breaker '\n'
