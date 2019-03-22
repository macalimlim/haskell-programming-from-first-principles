{-# LANGUAGE NoImplicitPrelude #-}

module Cipher where

import Control.Monad (return)
import Data.Bool     (otherwise)
import Data.Char     (Char)
import Data.Function (($))
import Data.Int      (Int)
import Data.List     (map)
import Data.Ord      ((<), (>))
import Data.String   (String)
import GHC.Enum      (pred, succ)
import GHC.Num       ((-))
import System.IO     (IO, getLine, putStrLn)
import Text.Read     (read)

shift :: (Char -> Char) -> Int -> Char -> Char
shift _ 0 c | c > 'z'   = 'a'
            | c < 'a'   = 'z'
            | otherwise = c
shift f n c | c > 'z'   = shift f n 'a'
            | c < 'a'   = shift f n 'z'
            | otherwise = shift f (n - 1) (f c)

shiftRight :: Int -> Char -> Char
shiftRight = shift succ

shiftLeft :: Int -> Char -> Char
shiftLeft = shift pred

caesar :: Int -> String -> String
caesar n = map (shiftRight n)

unCaesar :: Int -> String -> String
unCaesar n = map (shiftLeft n)

main :: IO ()
main         = do
  word <- getLine
  n    <- getLine
  let number = read n
  putStrLn $ caesar number word
