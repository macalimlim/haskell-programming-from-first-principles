{-# LANGUAGE NoImplicitPrelude #-}

module Jammin where

import Data.Bool     (Bool, otherwise)
import Data.Eq       (Eq, (==))
import Data.Function (($))
import Data.Int      (Int)
import Data.List     (foldr, groupBy, head, map, sortBy, sum)
import Data.Ord      (Ord, Ordering, compare, (>))
import Text.Show     (Show)

data Fruit = Peach
           | Plum
           | Apple
           | Blackberry
           deriving (Eq, Ord, Show)

data JamJars = Jam Fruit Int
             deriving (Eq, Ord, Show)

data JamJars' = Jam'
              { fruit :: Fruit
              , num   :: Int
              } deriving (Eq, Ord, Show)

-- cardinality of JamJars? minBound of Int + maxBound of Int + 4 (for Fruit)

row1 :: JamJars'
row1 = Jam' Peach 1

row2 :: JamJars'
row2 = Jam' Plum 21

row3 :: JamJars'
row3 = Jam' Apple 321

row4 :: JamJars'
row4 = Jam' Blackberry 4321

allJam :: [JamJars']
allJam = [row1, row2, row3, row4]

totalJam :: Int
totalJam = sum $ map num allJam

maxJam :: JamJars' -> JamJars' -> JamJars'
maxJam j@(Jam' _ n) j'@(Jam' _ n') | n > n'    = j
                                   | otherwise = j'

mostRow :: JamJars'
mostRow = foldr maxJam (head allJam) allJam

compareJam :: JamJars' -> JamJars' -> Ordering
compareJam jam jam' = fruit jam `compare` fruit jam'

sortedJams :: [JamJars']
sortedJams = sortBy compareJam allJam

eqJam :: JamJars' -> JamJars' -> Bool
eqJam (Jam' f _) (Jam' f' _) = f == f'

groupJams :: [[JamJars']]
groupJams = groupBy eqJam allJam
