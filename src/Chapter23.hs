{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Chapter23 where

import Control.Applicative (Applicative (pure, (<*>)))
import Control.Monad       (Monad (return, (>>=)))
import Data.Bool           (otherwise)
import Data.Eq             (Eq)
import Data.Function       (($))
import Data.Functor        (Functor (fmap))
import Data.Int            (Int)
import Data.List           ((++))
import Data.Ord            ((>=))
import GHC.Err             (error)
import GHC.Num             ((+))
import System.Random       (StdGen, randomR)
import Text.Show           (Show, show)

data Die = DieOne
         | DieTwo
         | DieThree
         | DieFour
         | DieFive
         | DieSix
         deriving (Eq, Show)

intToDie :: Int -> Die
intToDie 1 = DieOne
intToDie 2 = DieTwo
intToDie 3 = DieThree
intToDie 4 = DieFour
intToDie 5 = DieFive
intToDie 6 = DieSix
intToDie n = error $ "intToDie got non 1-6 integer: " ++ show n

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty                     = go 0 0
  where go       :: Int    -> Int -> StdGen -> Int
        go sum count gen | sum >= 20 = count
                         | otherwise = let (die, nextGen) = randomR (1, 6) gen
                                       in go (sum + die) (count + 1) nextGen

{-

Exercises: Roll Your Own

1. Refactor rollsToGetTwenty into having the limit be a function
argument.

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN = undefined

-}

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n                        = go 0 0
  where go sum count gen | sum >= n  = count
                         | otherwise = let (die, nextGen) = randomR (1, 6) gen
                                       in go (sum + die) (count + 1) nextGen
{-

2. Change rollsToGetN to recording the series of die that occurred
in addition to the count.

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged = undefined

-}

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n    = go 0 (0, [])
  where go sum (count, ds) gen
          | sum >= n  = (count, ds)
          | otherwise = let (die, nextGen) = randomR (1, 6) gen
                        in go (sum + die) (count + 1, ds ++ [intToDie die]) nextGen

{-

Write State for yourself

Use the datatype definition from the beginning of this chapter, with
the name changed to avoid conflicts in case you have State imported
from the libraries transformers or mtl. We’re calling it Moi, because
we enjoy allusions to famous quotations 3 ; feel free to change the
name if you wish to protest absolute monarchy, just change them
consistently throughout.

-}

newtype Moi s a = Moi
                { runMoi :: s -> (a, s)
                }

{-

State Functor

Implement the Functor instance for State.

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = ???

Prelude> runMoi ((+1) <$> (Moi $ \s -> (0, s))) 0
(1,0)

-}

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi h
    where (x, s) = g s
          h s'   = (f x, s')

{-

State Applicative

Write the Applicative instance for State.

instance Applicative (Moi s) where
  pure  :: a        -> Moi s a
  pure a              = ???
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = ???

-}

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a      = Moi f
    where f s = (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi h
    where (x, s)      = g s
          (fn, s')    = f s'
          h s''       = (fn x, s'')

{-

State Monad

Write the Monad instance for State.

instance Monad (Moi s) where
  return        = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = ???

-}

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g  = Moi h
    where (x, s) = f s
          h      = runMoi $ g x

{-

Chapter exercises

Write the following functions. You’ll want to use your own State type
for which you’ve defined the Functor, Applicative, and Monad.

1. Construct a State where the state is also the value you return.

get :: State s s
get = ???

Expected output

Prelude> runState get "curryIsAmaze"
("curryIsAmaze","curryIsAmaze")

-}

get :: Moi s s
get         = Moi f
  where f s = (s, s)

{-

2. Construct a State where the resulting state is the argument
provided and the value is defaulted to unit.

put :: s -> State s ()
put s = ???

Prelude> runState (put "blah") "woot"
((),"blah")

-}

put :: s -> Moi s ()
put s       = Moi f
  where f _ = ((), s)

{-

3. Run the State with s and get the state that results.

exec :: State s a -> s -> s
exec (State sa) s = ???

Prelude> exec (put "wilma") "daphne"
"wilma"

Prelude> exec get "scooby papu"
"scooby papu"

-}

exec :: Moi s a -> s -> s
exec (Moi f) s  = s'
  where (_, s') = f s

{-

4. Run the State with s and get the value that results.

eval :: State s a -> s -> a
eval (State sa) = ???

Prelude> eval get "bunnicula"
"bunnicula"

Prelude> eval get "stake a bunny"
"stake a bunny"

-}

eval :: Moi s a -> s -> a
eval (Moi f) s = x
  where (x, _) = f s

{-

5. Write a function which applies a function to create a new State.

modify :: (s -> s) -> State s ()
modify = undefined

Should behave like the following:

Prelude> runState (modify (+1)) 0
((),1)

Prelude> runState (modify (+1) >> modify (+1)) 0
((),2)

Note you don’t need to compose them, you can just throw away
the result because it returns unit for a anyway.

-}

modify :: (s -> s) -> Moi s ()
modify f    = Moi g
  where g s = ((), f s)
