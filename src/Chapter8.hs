{-# LANGUAGE NoImplicitPrelude #-}

module Chapter8 where

import Data.Bool     (otherwise)
import Data.Eq       (Eq, (==))
import Data.Function (flip, ($))
import Data.Int      (Int)
import Data.List     (map, (++))
import Data.Ord      (Ord, (<), (<=), (>))
import Data.String   (String)
import GHC.Integer   (Integer)
import GHC.Num       (Num, (+), (-))
import GHC.Real      (Integral, div)
import Text.Show     (Show)

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 _ b = b
applyTimes n f b = f (applyTimes (n - 1) f b)

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times = applyTimes times (+ 1)

{-

Intermission: Exercise

Write out the evaluation of the following. It might be a little less noisy
if you do so with the form that didn’t use (.) .

applyTimes 5 (+ 1) 5
(+ 1) (applyTimes (5 - 1) (+ 1) 5)
(+ 1) (applyTimes 4 (+ 1) 5)
(+ 1) ((+ 1) (applyTimes (4 - 1) (+ 1) 5))
(+ 1) ((+ 1) (applyTimes 3 (+ 1) 5))
(+ 1) ((+ 1) ((+ 1) (applyTimes (3 - 1) (+ 1) 5)))
(+ 1) ((+ 1) ((+ 1) (applyTimes 2 (+ 1) 5)))
(+ 1) ((+ 1) ((+ 1) ((+ 1) (applyTimes (2 - 1) (+ 1) 5))))
(+ 1) ((+ 1) ((+ 1) ((+ 1) (applyTimes 1 (+ 1) 5))))
(+ 1) ((+ 1) ((+ 1) ((+ 1) ((+ 1) (applyTimes (1 - 1) (+ 1) 5)))))
(+ 1) ((+ 1) ((+ 1) ((+ 1) ((+ 1) (applyTimes 0 (+ 1) 5)))))
(+ 1) ((+ 1) ((+ 1) ((+ 1) ((+ 1) (5)))))
(+ 1) ((+ 1) ((+ 1) ((+ 1) (6))))
(+ 1) ((+ 1) ((+ 1) (7)))
(+ 1) ((+ 1) (8))
(+ 1) (9)
10


Chapter Exercises

Review of types

1. What is the type of [[True, False], [True, True], [False, True]] ?

a) Bool

b) mostly True

c) [a]

d) [[Bool]]

=> d


2. Which of the following has the same type as
[[True, False], [True, True], [False, True]] ?

a) [(True, False), (True, True), (False, True)]

b) [[3 == 3], [6 > 5], [3 < 4]]

c) [3 == 3, 6 > 5, 3 < 4]

d) ["Bool", "more Bool", "Booly Bool!"]

=> b


3. For the following function

func :: [a] -> [a] -> [a]
func x y = x ++ y

which of the following is true?

a) x and y must be of the same type

b) x and y must both be lists

c) if x is a String then y must be a String

d) all of the above

=> d


4. For the func code above, which is a valid application of func to
both of its arguments?

a) func "Hello World"

b) func "Hello" "World"

c) func [1, 2, 3] "a, b, c"

d) func ["Hello", "World"]

=> b


Reviewing currying

Given the following definitions, tell us what value results from further
applications.

-}

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- fill in the types

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

{-

1. What is the value of appedCatty "woohoo!" ? Try to determine
the answer for yourself, then test in the REPL.

=> "woops mrow woohoo!"


2. frappe "1"

=> "1 mrow haha"


3. frappe (appedCatty "2")

=> "woops mrow 2 mrow haha"


4. appedCatty (frappe "blue")

=> "woops mrow blue mrow haha"


5. cattyConny (frappe "pink")
              (cattyConny "green" (appedCatty "blue"))

=> "pink mrow haha mrow green mrow woops mrow blue"


6. cattyConny (flippy "Pugs" "are") "awesome"

=> "are mrow Pugs mrow awesome"


Recursion

1. Write out the steps for reducing dividedBy 15 2 to its final
answer according to the Haskell code.

-}

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom              = go num denom 0
  where go n d count | n < d     = (count, n)
                     | otherwise = go (n - d) d (count + 1)

{-

divideBy 15 2
go 15 2 0
go (15 - 2) 2 (0 + 1)
go 13 2 1
go (13 - 2) 2 (1 + 1)
go 11 2 2
go (11 - 2) 2 (2 + 1)
go 9 2 3
go (9 - 2) 2 (3 + 1)
go 7 2 4
go (7 - 2) 2 (4 + 1)
go 5 2 5
go (5 - 2) 2 (5 + 1)
go 3 2 6
go (3 - 2) 2 (6 + 1)
go 1 2 7
(7, 1)


2. Write a function that recursively sums all numbers from 1 to n,
n being the argument. So that if n was 5, you’d add 1 + 2 + 3 + 4 +
5 to get 15. The type should be (Eq a, Num a) => a -> a .

-}

recSum :: (Eq a, Num a) => a -> a
recSum n | n == 0    = n
         | otherwise = n + recSum (n - 1)

{-

3. Write a function that multiplies two integral numbers using
recursive summation. The type should be
(Integral a) => a -> a -> a .

-}

recMul :: Integral a => a -> a -> a
recMul x 1 = x
recMul x y = x + recMul x (y - 1)

{-

Fixing dividedBy

Our dividedBy function wasn’t quite ideal. For one thing. It was a
partial function and doesn’t return a result (bottom) when given a
divisor that is 0 or less.

Using the pre-existing div function we can see how negative numbers
should be handled:

Prelude> div 10 2
5

Prelude> div 10 (-2)
-5

Prelude> div (-10) (-2)
5

Prelude> div (-10) (2)
-5

-}

dividedBy' :: Integral a => a -> a -> (a, a)
dividedBy' num denom             = go num denom 0
  where go _ d _     | d <= 0    = (0, 0)
        go n d count | n < d     = (count, n)
                     | otherwise = go (n - d) d (count + 1)

{-

The next issue is how to handle zero. Zero is undefined for division in
math, so really we ought to use a datatype that lets us say there was no
sensible result when the user divides by zero. If you need inspiration,
consider using the following datatype to handle this.

data DividedResult = Result Integer
                   | DividedByZero

-}

data DividedResult = Result Integer
                   | DividedByZero
                   deriving Show

myDiv :: Integer -> Integer -> DividedResult
myDiv _ 0 = DividedByZero
myDiv n d = Result $ n `div` d

{-

McCarthy 91 function

We’re going to describe a function in English, then in math notation,
then show you what your function should return for some test inputs.
Your task is to write the function in Haskell.
The McCarthy 91 function yields x − 10 when x > 100 and 91 otherwise.
The function is recursive.

        ⎧
        ⎪ n − 10         if n > 100
MC(n) = ⎨
        ⎪ MC(MC(n + 11)) if n ≤ 100
        ⎩

mc91 = undefined

-}

mc91 :: (Num a, Ord a) => a -> a
mc91 x | x > 100   = x - 10
       | otherwise = 91

{-

You haven’t seen map yet, but all you need to know right now is that it
applies a function to each member of a list and returns the resulting
list. It’ll be explained in more detail in the next chapter.

Prelude> map mc91 [95..110]
[91,91,91,91,91,91,91,92,93,94,95,96,97,98,99,100]

-}

res :: [Int]
res = map mc91 [95..110]

{-

Numbers into words

module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = undefined

digits :: Int -> [Int]
digits n = undefined

wordNumber :: Int -> String
wordNumber n = undefined

Here undefined is a placeholder to show you where you need to fill in
the functions. The n to the right of the function names is the argument
which will be an integer.

Fill in the implementations of the functions above so that wordNumber
returns the English word version of the Int value. You will first write a
function that turns integers from 1-9 into their corresponding English
words, ”one,” ”two,” and so on. Then you will write a function that
takes the integer, separates the digits, and returns it as a list of integers.
Finally you will need to apply the first function to the list produced by
the second function and turn it into a single string with interspersed
hyphens.

We’ve laid out multiple functions for you to consider as you tackle
the problem. You may not need all of them, depending on how you
solve it–these are just suggestions. Play with them and look up their
documentation to understand them in deeper detail.

You will probably find this difficult.

div :: Integral a => a -> a -> a

mod :: Integral a => a -> a -> a

map :: (a -> b) -> [a] -> [b]

concat :: [[a]] -> [a]

intersperse :: a -> [a] -> [a]

(++) :: [a] -> [a] -> [a]

(:[]) :: a -> [a]

Also consider:

Prelude> div 135 10
13

Prelude> mod 135 10
5

Prelude> div 13 10
1

Prelude> mod 13 10
3

Here is what your output should look in the REPL when it’s working:

Prelude> wordNumber 12324546
"one-two-three-two-four-five-four-six"

=> see wordNumber.hs

-}
