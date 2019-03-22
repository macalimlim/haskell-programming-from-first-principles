{-# LANGUAGE NoImplicitPrelude #-}

module WordNumber where

import Data.Char     (Char)
import Data.Function ((.))
import Data.Int      (Int)
import Data.List     (intercalate, map)
import Data.String   (String)
import Text.Read     (read)
import Text.Show     (show)

readCons         :: Char   -> Int
readCons = (read :: String -> Int) . (: [])

digitToWord :: Int -> String
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "zero"

digits :: Int -> [Int]
digits = map readCons . show

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits
