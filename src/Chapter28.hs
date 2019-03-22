{-# LANGUAGE NoImplicitPrelude #-}

module Chapter28 where

import Criterion.Main (bench, defaultMain, whnf)
import Data.Bool      (Bool)
import Data.Eq        (Eq)
import Data.Function  (const, ($), (.))
import Data.List      (iterate, take)
import Data.Maybe     (Maybe)
import GHC.Err        (undefined)
import GHC.Int        (Int)
import GHC.Num        ((+), (-))
import System.IO      (IO)
import Text.Show      (Show)

import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as UV

{-

Exercise: Benchmark Practice

Make a benchmark to prove for yourself whether Map and Set have
similar performance. Try operations other than membership testing,
such as insertion or unions.

-}

bumpIt :: (Int, Int) -> (Int, Int)
bumpIt (i, v) = (i + 1, v + 1)

myMap :: M.Map Int Int
myMap          =  M.fromList $ take 10000 stream
  where stream = iterate bumpIt (0, 0)

mapInsert :: (Int, Int) -> M.Map Int Int
mapInsert (k, v) = M.insert k v myMap

mySet :: S.Set Int
mySet          =  S.fromList $ take 10000 stream
  where stream = iterate (+1) 0

-- for the benchmarks see main function below

{-

Exercises: Vector

Setup a benchmark harness with criterion to profile how much memory
boxed and unboxed vectors containing the same data uses. You
can combine this with a benchmark to give it something to do for a
few seconds. We’re not giving you a lot because you’re going to have
to learn to read documentation and source code anyway.

-}

myVector :: V.Vector Int
myVector = V.fromList [1..99999]

myUnboxedVector :: UV.Vector Int
myUnboxedVector = UV.fromList [1..99999]

-- for the benchmarks see main function below

{-

Difference List

Lists are really nice, but they don’t append or concatenate cheaply.
We covered Sequence as one potential solution to this, but there’s a
simpler data structure that solves slow appending specifically, the
difference list!

Rather than justify and explain difference lists, part of the exercise
is figuring out what it does and why (although feel free to look up the
documentation on Hackage). Attempt the exercise before resorting
to the tutorial in the follow-up reading. First, the DList type is built
on top of ordinary lists, but it’s a function:


-}

newtype DList a = DL
  { unDL :: [a] -> [a]
  }

{-

The API that follows is based on code by Don Stewart and Sean
Leather. Here’s what you need to write:

1.
empty :
: DList a
empty = undefined
{-# INLINE empty #-}

2.
singleton :: a -> DList a
singleton = undefined
{-# INLINE singleton #-}

3.
toList :: DList a -> [a]
toList = undefined
{-# INLINE toList #-}

4.
-- Prepend a single element to a dlist.
infixr `cons`
cons
 :: a -> DList a -> DList a
cons x xs
 = DL ((x:) . unDL xs)
{-# INLINE cons #-}

5.
-- Append a single element to a dlist.
infixl `snoc`
snoc :: DList a -> a -> DList a
snoc = undefined
{-# INLINE snoc #-}

6.
-- Append dlists.
append :: DList a -> DList a -> DList a
append = undefined
{-# INLINE append #-}

What’s so nifty about DList is that cons, snoc, and append all take the
same amount of time no matter how long the dlist is. That is to say,
they take a constant amount of time rather than growing with the size
of the data structure.

Your goal is to get the following benchmark harness running with
the performance expected:

-}

empty :: DList a
empty = DL (const [])
{-# INLINE empty #-}

singleton :: a -> DList a
singleton x = DL (const [x])
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList xs = unDL xs []
{-# INLINE toList #-}

-- Prepend a single element to a dlist.
infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

-- Append a single element to a dlist.
infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL (unDL xs . (x:))
{-# INLINE snoc #-}

-- Append dlists.
append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i     = go i []
  where go 0 xs = xs
        go n xs = go (n-1) (n : xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs  = xs
        go n xs  = go (n - 1) (singleton n `append` xs)

-- for the benchmarks see main function below

{-

A simple queue

We’re going to write another data structure in terms of list, but this
time it’ll be a queue. The main feature of queues is that we can add
elements to the front cheaply and take items off the back of the queue
cheaply.

-- From Okasaki's Purely
-- Functional Data Structures
data Queue a = Queue
  { enqueue :: [a]
  , dequeue :: [a]
  } deriving (Eq, Show)

-- adds an item
push :: a -> Queue a -> Queue a
push = undefined

pop :: Queue a -> Maybe (a, Queue a)
pop = undefined

We’re going to give you less code this time, but your task is to
implement the above and write a benchmark comparing it against
performing alternating pushes and pops from a queue based on a
single list. Alternating so that you can’t take advantage of reversing
the list after a long series of pushes in order to perform a long series
of pops efficiently.

Don’t forget to handle the case where the dequeue is empty and
you need to shift items from the enqueue to the dequeue. You need
to do so without violating “first come, first served”.
Lastly, benchmark it against Sequence. Come up with a variety of
tests. Add additional operations for your Queue type if you want.

-}

-- From Okasaki's Purely
-- Functional Data Structures

class IQueue q where
  empty'  :: q a
  isEmpty :: q a -> Bool
  snoc'   :: q a -> a -> q a
  head    :: q a -> a
  tail    :: q a -> q a

data Queue a = Queue
  { enqueue :: [a]
  , dequeue :: [a]
  } deriving (Eq, Show)

-- adds an item
push :: a -> Queue a -> Queue a
push _ _ = undefined

pop :: Queue a -> Maybe (a, Queue a)
pop = undefined

main :: IO ()
main = defaultMain
  [ bench "map insert" $ whnf mapInsert (99999, 99999)
  , bench "set union" $ whnf S.union mySet
  , bench "boxed vector length" $ whnf V.length myVector
  , bench "unboxed vector length" $ whnf UV.length myUnboxedVector
  , bench "concat list" $ whnf schlemiel 123456
  , bench "concat dlist" $ whnf constructDlist 123456
  ]
