{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Strict            #-}

module Chapter27 where

import Data.Function (($))
import Data.Int      (Int)
import Data.Ord      ((<=))
import Data.String   (String)
import GHC.Err       (undefined)
import GHC.Num       ((+), (-))
import System.IO     (IO, print)
import Text.Show     (Show)

{-

Exercises: Evaluate
Expand the expression in as much detail as possible. Then, work
outside-in to see what the expression evaluates to.

1. const 1 undefined
=> 1

2. const undefined 1
=> exception

3. flip const undefined 1
=> 1

4. flip const 1 undefined
=> exception

5. const undefined undefined
=> exception

6. foldr const 'z' ['a'..'e']
=> 'a'

7. foldr (flip const) 'z' ['a'..'e']
=> 'z'


Chapter Exercises

Strict List

Try messing around with the following list type and compare what
it does with the bang-patterned list variants we experimented with
earlier:

-}

data List a = Nil
            | Cons a (List a)
            deriving (Show)

take' :: Int -> List a -> List a
take' n _           | n <= 0 = Nil
take' _ Nil                  = Nil
take' n (Cons x xs)          = Cons x (take' (n - 1) xs)

map' :: (a -> b) -> List a -> List b
map' _ Nil         = Nil
map' f (Cons x xs) = Cons (f x) (map' f xs)

repeat' :: a -> List a
repeat' x  = xs
  where xs = Cons x xs

main' :: IO ()
main' = print $ take' 10 $ map' (+1) (repeat' 1)

{-

What will :sprint output?

We show you a definition or multiple definitions, you determine
what :sprint will output when passed the bindings listed in your head
before testing it.

1. let x = 1
=> x     = _

2. let x = ['1']
=> x     = "1"

3. let x = [1]
=> x     = _

4. let x = 1 :: Int
=> x     = 1

5. let f = \x -> x
   let x = f 1
=> x     = _

6. let f :: Int -> Int; f = \x -> x
   let x                  = f 1
=> x                      = _


Will printing this expression result in bottom?

1. snd (undefined, 1)
=> no

2. let x = undefined
   let y = x `seq` 1 in snd (x, y)
=> yes

3. length $ [1..5] ++ undefined
=> yes

4. length $ [1..5] ++ [undefined]
=> no

5. const 1 undefined
=> no

6. const 1 (undefined `seq` 1)
=> no

7. const undefined 1
=> yes


Make the expression bottom

Using only bang patterns or seq, make the code bottom out when
executed.

-}

x' :: a
x' = undefined

y' :: String
y' = "blah"

snd' :: (a, b) -> b
snd' (!_, y) = y

main :: IO ()
main = print (snd' (x', y'))
