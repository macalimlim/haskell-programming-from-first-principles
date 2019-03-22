{-# LANGUAGE NoImplicitPrelude #-}

module Chapter7 where

import Data.Bool     (Bool (False, True), otherwise)
import Data.Char     (Char)
import Data.Eq       (Eq, (==))
import Data.Function (flip, ($))
import Data.List     (reverse)
import Data.Ord      (Ord, Ordering (EQ, GT, LT), compare, min, (<), (>), (>=))
import Data.String   (String)
import Data.Tuple    (fst)
import GHC.Err       (undefined)
import GHC.Integer   (Integer)
import GHC.Num       (Num, (*), (+), (-))
import GHC.Real      (Fractional, Integral, div, divMod, even, mod, odd, (/))

{-

Intermission: Exercises

Note the following exercises are from source code files, not written
for use directly in the REPL. Of course, you can change them to test
directly in the REPL if you prefer.

1. Which (two or more) of the following are equivalent?

a) mTh x y z = x * y * z

b) mTh x y = \z -> x * y * z

c) mTh x = \y -> \z -> x * y * z

d) mTh = \x -> \y -> \z -> x * y * z

=> a, b, c and d


2. The type of mTh (above) is Num a => a -> a -> a -> a
Which is the type of mTh 3 ?

a) Integer -> Integer -> Integer

b) Num a => a -> a -> a -> a

c) Num a => a -> a

d) Num a => a -> a -> a

=> d


3. Next, we’ll practice writing anonymous lambda syntax.
For example, one could rewrite:

addOne x = x + 1

Into:

addOne = \x -> x + 1

Try to make it so it can still be loaded as a top-level definition by
GHCi. This will make it easier to validate your answers.

a) Rewrite the f function in the where clause.
addOneIfOdd n = case odd n of
  True  -> f n
  False -> n
  where f n   = n + 1

-}

f :: Num a => a -> a
f = \n'         -> n' + 1

addOneIfOdd :: Integral a => a -> a
addOneIfOdd n = if odd n then f n else n

{-

b) Rewrite the following to use anonymous lambda syntax:
addFive x y = (if x > y then y else x) + 5

-}

addFive :: (Num a, Ord a) => a -> a -> a
addFive = \x y                 -> min x y + 5

{-

c) Rewrite the following so that it doesn’t use anonymous
lambda syntax:
mflip f = \x -> \y -> f y x

-}

mflip :: (a -> b -> c) -> b -> a -> c
mflip fn x y = fn y x


{-

Intermission: Exercises

1. Given the following declarations

-}

k :: (a, b) -> a
k (x, _) = x

k1 :: Integer
k1 = k (4 - 1, 10)

k2 :: String
k2 = k ("three", 1 + 2)

k3 :: Integer
k3 = k (3, True)

{-}

a) What is the type of k ?
=> k :: (a, b) -> a

b) What is the type of k2 ? Is it the same type as k1 or k3 ?
=> k2 :: String
=> no

c) Of k1, k2, k3 , which will return the number 3 as the result?
=> k1 and k3


2. Fill in the definition of the following function:
-- Remember: Tuples have the same syntax for their
-- type constructors and their data constructors.
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f = undefined

-}

f' :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f' (x, _, z) (x', _, z') = ((x, x'), (z, z'))


{-

Intermission: Exercises

We’re going to practice using case expressions by rewriting functions.
Some of these functions you’ve already seen in previous chapters (and
some you’ll see later using different syntax yet again!), but you’ll be
writing new versions now. Please note these are all written as they
would be in source code files, and we recommend you write your
answers in source files and then load into GHCi to check, rather than
trying to do them directly into the REPL.

First, rewrite if-then-else expressions into case expressions.

1. The following should return x when x is greater than y .

functionC x y = if (x > y) then x else y

-}

functionC :: Ord a => a -> a -> a
functionC x y = if x > y then x else y

functionC' :: Ord a => a -> a -> a
functionC' x y = case x > y of
                   True  -> x
                   False -> y

{-

2. The following will add 2 to even numbers and otherwise simply
return the input value.

ifEvenAdd2 n = if even n then (n+2) else n

-}

ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 n = if even n then n + 2 else n

ifEvenAdd2' :: Integral a => a -> a
ifEvenAdd2' n = case even n of
                  True         -> n + 2
                  False        -> n

{-

The next exercise doesn’t have all the cases covered. See if you can fix
it.

1. The following compares a value, x , to zero and returns an indica-
tor for whether x is a postive number or negative number. But
what if x is 0? You may need to play with the compare function
a bit to find what to do.

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1

-}

nums :: (Num a, Ord a) => a -> a
nums x =
  case compare x 0 of
    LT                      -> -1
    GT                      -> 1
    EQ                      -> 0


{-

Intermission: Exercises

Given the following definitions tell us what value results from further
applications. When you’ve written down at least some of the answers
and think you know what’s what, type the definitions into a file and
load them in GHCi to test your answers.

-- Types not provided, try filling them in yourself.

-}

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = flip dodgy 2

{-

1. For example, given the expression dodgy 1 0 , what do you think
will happen if we evaluate it? If you put the definitions in a file
and load them in GHCi, you can do the following to see the
result.

Prelude> dodgy 1 0
1

Now attempt to determine what the following expressions reduce
to. Do it in your head, verify in your REPL after you think you
have an answer.

2. dodgy 1 1
=> 11

3. dodgy 2 2
=> 22

4. dodgy 1 2
=> 21

5. dodgy 2 1
=> 12

6. oneIsOne 1
=> 11

7. oneIsOne 2
=> 21

8. oneIsTwo 1
=> 21

9. oneIsTwo 2
=> 22

10. oneIsOne 3
=> 31

11. oneIsTwo 3
=> 23


Intermission: Exercises

1. It is probably clear to you why you wouldn’t put an otherwise
in your top-most guard, but try it with avgGrade anyway and
see what happens. It’ll be more clear if you rewrite it as an actual
otherwise match: | otherwise = 'F' . What happens now if
you pass a 90 as an argument? 75? 60?

-}

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | otherwise = 'F'
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59  = 'F'
  where y     = x / 100

-- always 'F' or Fail

{-

2. What happens if you take avgGrade as it is written and reorder
the guards? Does it still typecheck and work the same? Try mov-
ing | y >= 0.7 = 'C' and passing it the argument 90, which
should be an ‘A.’ Does it return an ‘A’?

-}

avgGrade' :: (Fractional a, Ord a) => a -> Char
avgGrade' x
  | y < 0.59  = 'F'
  | y >= 0.59 = 'D'
  | y >= 0.7  = 'C'
  | y >= 0.8  = 'B'
  | y >= 0.9  = 'A'
  | otherwise = 'F'
  where y     = x / 100

-- will typecheck
-- but doesn't work the same
-- avgGrade' 90 returns 'D'

{-

3. The following function returns

-}

pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise        = False

{-

a) xs written backwards when it’s True
b) True when xs is a palindrome
c) False when xs is a palindrome
d) False when xs is reversed

=> b

4. What types of arguments can pal take?
=> list of a type constrained to Eq

5. What is the type of the function pal?
=> pal :: Eq a => [a] -> Bool

6. The following function returns

-}

numbers :: (Num a, Ord a) => a -> a
numbers x
  | x < 0     = -1
  | x == 0    = 0
  | x > 0     = 1
  | otherwise = 0

{-

a) the value of its argument plus or minus 1
b) the negation of its argument
c) an indication of whether its argument is a positive or negative number or
zero
d) binary machine language

=> c

7. What types of arguments can numbers take?
=> any type constrained to Num and Ord

8. What is the type of the function numbers?
=> numbers :: (Num a, Ord a) => a -> a


Chapter Exercises

Multiple choice

1. A polymorphic function

a) changes things into sheep when invoked

b) has multiple arguments

c) has a concrete type

d) may resolve to values of different types, depending on inputs

=> d


2. Two functions named f and g have types
Char   -> String
and
String -> [String]
respectively. The composed function g . f has the type

a) Char -> String

b) Char -> [String]

c) [[String]]

d) Char -> String -> [String]

=> b


3. A function f has the type Ord a => a -> a -> Bool and we
apply it to one numeric value. What is the type now?

a) Ord a => a -> Bool

b) Num -> Num -> Bool

c) Ord a => a -> a -> Integer

d) (Ord a, Num a) => a -> Bool

=> d

-}

myo :: Ord a => a -> a -> Bool
myo = undefined

{-

4. A function with the type (a -> b) -> c

a) requires values of three different types

b) is a higher-order function

c) must take a tuple as its first argument

d) has its parameters in alphabetical order

=> b


5. Given the following definition of f , what is the type of f True ?

-}

f'' :: a -> a
f'' x = x

{-

a) f True :: Bool

b) f True :: String

c) f True :: Bool -> Bool

d) f True :: a

=> a


Let’s write code

1. The following function returns the tens digit of an integral argument.

-}

tensDigit :: Integral a => a -> a
tensDigit x   = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

{-

a) First, rewrite it using divMod .

-}

tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where d    = fst $ x `divMod` 10

{-

b) Does the divMod version have the same type as the original
version?
=> yes

c) Next, let’s change it so that we’re getting the hundreds digit
instead. You could start it like this (though that may not be
the only possibility):

hunsD x   = d2
  where d = undefined
  ...

-}

hunsD :: Integral a => a -> a
hunsD x    = d2
  where d  = x `div` 100
        d2 = d `mod` 100

{-

2. Implement the function of the type a -> a -> Bool -> a once
each using a case expression and once with a guard.

foldBool :: a -> a -> Bool -> a
foldBool = error "Error: Need to implement foldBool!"

-}

foldBool :: a -> a -> Bool -> a

foldBool x y b = case b of
                   True  -> x
                   False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y b | b         = x
                | otherwise = y

{-

The result is semantically similar to if-then-else expressions
but syntactically quite different. Here is the pattern matching
version to get you started:

foldBool3 :: a -> a -> Bool -> a
foldBool3 x y True  = x
foldBool3 x y False = y

-}

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ True  = x
foldBool3 _ y False = y

foldBool3' :: a -> a -> Bool -> a
foldBool3' x y b = if b then x else y

{-

3. Fill in the definition. Note that the first argument to our function
is also a function which can be applied to values. Your second
argument is a tuple, which can be used for pattern matching:

g :: (a -> b) -> (a, c) -> (b, c)
g = undefined

-}

g :: (a -> b) -> (a, c) -> (b, c)
g fu (x, y) = (fu x, y)

{-

4. For this next exercise, you’ll experiment with writing pointfree
versions of existing code. This involves some new information,
so read the following explanation carefully.

Typeclasses are dispatched by type. Read is a typeclass like Show ,
but it is the dual or “opposite” of Show . In general, the Read
typeclass isn’t something you should plan to use a lot, but this ex-
ercise is structured to teach you something about the interaction
between typeclasses and types.

The function read in the Read typeclass has the type:

read :: Read a => String -> a

Notice a pattern?

read :: Read a => String -> a
show :: Show a => a      -> String

Write the following code into a source file. Then load it and
run it in GHCi to make sure you understand why the evaluation
results in the answers you see.

-- arith4.hs
module Arith4 where
-- id :: a -> a
-- id x = x

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

main = do print (roundTrip 4)
          print (id 4)

=> see Arith4.hs

-}
