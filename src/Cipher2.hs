{-# LANGUAGE NoImplicitPrelude #-}

module Cipher2 where

import Data.Bool     (otherwise)
import Data.Char     (Char, chr, isLower, ord)
import Data.Eq       (Eq, (==))
import Data.Function (($))
import Data.Int      (Int)
import Data.List     (cycle, drop, head, length, map, minimum, sum, take, zip,
                      (++))
import Data.Ord      ((>))
import Data.String   (String)
import GHC.Exts      (Float)
import GHC.Num       ((*), (+), (-))
import GHC.Real      (fromIntegral, mod, (/), (^))
import System.IO     (IO, getLine, putStrLn)

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: String -> String -> String
encode key msg = [shift (let2int k) c | (k,c) <- zip (cycle key) msg]

decode :: String -> String -> String
decode key msg = [shift (26 - let2int k) c | (k,c) <- zip (cycle key) msg]

type FrequencyTable = [Float]

table :: FrequencyTable
table =  [ 8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0
         , 6.1, 7.0, 0.2, 0.8,  4.0, 2.4, 6.7
         , 7.5, 1.9, 0.1, 6.0,  6.3, 9.1, 2.8
         , 1.0, 2.4, 0.2, 2.0,  0.1]

lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs  = [percent (count x xs) n | x <- ['a'..'z']]
  where n = lowers xs

indexOfCoincidence :: String -> Float
indexOfCoincidence xs = numer / denom
  where counts        = map (`count` xs) ['a'..'z']
        n             = sum counts
        numer         = fromIntegral (sum [f * (f - 1) | f <- counts])
        denom         = fromIntegral (n * (n - 1))

passwordLength :: String -> Int
passwordLength xs | ic  > 0.06552 =  0
                  | ic  > 0.05185 =  1
                  | ic  > 0.04730 =  2
                  | ic  > 0.04502 =  3
                  | ic  > 0.04365 =  4
                  | ic  > 0.04274 =  5
                  | ic  > 0.04209 =  6
                  | ic  > 0.04160 =  7
                  | ic  > 0.04122 =  8
                  | ic  > 0.04092 =  9
                  | otherwise     = 10
  where ic                        = indexOfCoincidence xs

chisqr :: [Float] -> [Float] -> Float
chisqr oss es =  sum [((o - e) ^ 2) / e | (o, e) <- zip oss es]

rotate :: Int -> [a] -> [a]
rotate n xs =  drop n xs ++ take n xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs =  [i | (x', i) <- zip xs [0..], x == x']

bestmatch :: String -> Int
bestmatch xs   = head (positions (minimum chitab) chitab)
  where chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs

separateAlphabets :: String -> Int -> [String]
separateAlphabets xs n = [[ c | (c, p) <- zip xs (cycle [0..n - 1]), p `mod` n == i] | i <- [0..n - 1]]

findKey :: String -> String
findKey msg    = [int2let (bestmatch s) | s <- alphas]
  where alphas = separateAlphabets msg cnt
        cnt    = passwordLength msg

crack :: String -> String
crack cipherText = decode (findKey cipherText) cipherText

cipher :: String -> String
cipher cipherText = encode (findKey cipherText) cipherText

main :: IO ()
main = do word <- getLine
          putStrLn $ cipher word
