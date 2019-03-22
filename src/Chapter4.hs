{-# LANGUAGE NoImplicitPrelude #-}

module Chapter4 where

import Data.Bool   (Bool)
import Data.Eq     (Eq, (==))
import Data.Int    (Int)
import Data.List   (length, reverse)
import Data.Ord    ((>=))
import Data.String (String)
import Data.Tuple  (fst, snd)
import GHC.Err     (undefined)
import GHC.Integer (Integer)
import GHC.Num     (Num, negate, (+))
import Text.Show   (Show)

{-

Intermission: Exercises

Given the following datatype, answer the following questions:
data Mood = Blah | Woot deriving Show

1. What is the type constructor, or name of this type?
=> Mood

2. If the function requires a Mood value, what are the values you
could possibly use there?
=> Blah and Woot

3. We are trying to write a function changeMood to change Chris’s
mood instantaneously. So far, we’ve written a type signature
changeMood :: Mood -> Woot . What’s wrong with that?

=> it should be changeMood :: Mood -> Mood

4. Now we want to write the function that changes his mood. Given
an input mood, it gives us the other one. Fix any mistakes and
complete the function:

changeMood Mood = Woot
changeMood _    = Blah

changeMood Blah = Woot
changeMood Woot = Blah

5. Enter all of the above - datatype (including the “deriving Show”
bit), your corrected type signature, and the corrected function
into a source file. Load it and run it in GHCi to make sure you
got it right.

-}

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

{-

Intermission: Exercises

The following lines of code may have mistakes — some of them won’t
compile! You know what you need to do.

1. not True && true
=> will not compile
=> not True && True

2. not (x = 6)
=> will not compile
=> let x  = 5
=> not $ x == 6

3. (1 * 2) > 5
=> will compile

4. [Merry] > [Happy]
=> will not compile
=> ["Merry"] > ["Happy"]

5. [1, 2, 3] ++ "look at me!"
=> will not compile
=> ['1', '2', '3'] ++ "look at me!"

Chapter Exercises

As in previous chapters, you will gain more by working out the answer
before you check what GHCi tells you, but be sure to use your REPL
to check your answers to the following exercises. Also, you will need
to have the awesome, alsoAwesome, and allAwesome code from above
in scope for this REPL session. For convenience of reference, here
are those values again:

-}

awesome :: [String]
awesome = ["Papuchon", "curry", ":)"]

alsoAwesome :: [String]
alsoAwesome = ["Quake", "The Simons"]

allAwesome :: [[String]]
allAwesome = [awesome, alsoAwesome]

{-

length is a function that takes a list and returns a result that tells how
many items are in the list.

1. Given the definition of length above, what would the type signa-
ture be? How many arguments, of what type does it take? What
is the type of the result it evaluates to?
=> length :: [a] -> Int
=> a list of a type

2. What are the results of the following expressions?

a) length [1, 2, 3, 4, 5]
=> 5

b) length [(1, 2), (2, 3), (3, 4)]
=> 3

c) length allAwesome
=> 2

d) length (concat allAwesome)
=> 5

3. Given what we know about numeric types and the type signature
of length , look at these two expressions. One works and one
returns an error. Determine which will return an error and why.
(n.b., If you’re checking the type signature of length in GHC
7.10, you will find Foldable t => t a representing [a] , as with
concat in the previous chapter. Again, consider Foldable t to
represent a list here, even though list is only one of the possible
types. We will explain it in detail later.)

Prelude> 6 / 3
=> not an error
-- and
Prelude> 6 / length [1, 2, 3]
=> error! because there is no instance of Fractional for Int. type `:i Int`
inside the ghci

4. How can you fix the broken code from the preceding exercise
using a different division function/operator?
=> div 6 $ length [1, 2, 3]

5. What is the type of the expression 2 + 3 == 5?
expect as a result?
=> Bool
=> True

6. What is the type and expected result value of the following:
Prelude> let x = 5
Prelude> x + 3 == 5
=> False

7. Below are some bits of code. Which will work? Why or why not?
If they will work, what value would these reduce to?

Prelude> length allAwesome == 2
=> True

Prelude> length [1, 'a', 3, 'b']
=> doesn't work
=> 1 and 3 are not characters. a list should contain values of one type

Prelude> length allAwesome + length awesome
=> 5

Prelude> (8 == 8) && ('b' < 'a')
=> False

Prelude> (8 == 8) && 9
=> doesn't work
=> 9 is not of type Bool

8. Write a function that tells you whether or not a given String (or
list) is a palindrome. Here you’ll want to use a function called
’reverse,’ a predefined function that does just what it sounds like.

reverse :: [a] -> [a]
reverse "blah"
"halb"

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = undefined

-}

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

{-

9. Write a function to return the absolute value of a number using
if-then-else
myAbs :: Integer -> Integer
myAbs = undefined

-}

myAbs :: Integer -> Integer
myAbs n = if n >= 0
             then n
             else negate n

{-

10. Fill in the definition of the following function, using
fst and snd :

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f = undefined

-}

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f ex wai = ((snd ex, snd wai), (fst ex, fst wai))

{-

Reading syntax

In the following examples, you’ll be shown syntactically incorrect
code. Type it in and try to correct it in your text editor, validating it
with GHC or GHCi.

1. Here, we want a function that adds 1 to the length of a string
argument and returns that result.

x         = (+)
F xs      = w 'x' 1
  where w = length xs

-}

x :: Num a => a -> a -> a
x = (+)

f' :: Num a => [a] -> Int
f' xs     = w `x` 1
  where w = length xs

{-

2. This is supposed to be the identity function, id .
\ X = x

-}

idz :: a -> a
idz ex = ex

{-

3. When fixed, this function will return 1 from the value [1, 2, 3].
Hint: you may need to refer back to the section about variables
conventions in “Hello Haskell” to refresh your memory of this
notation.
\ x : xs -> x

-}

headz :: [a] -> a
headz []      = undefined
headz (h : _) = h

{-

4. When fixed, this function will return 1 from the value (1, 2)
f (a b) = A

-}

f'' :: (a, b) -> a
f'' (ex, _) = ex

{-

Match the function names to their types

1. Which of the following types is the type of show ?

a) show a => a -> String

b) Show a -> a -> String

c) Show a => a -> String

=> c

2. Which of the following types is the type of (==) ?

a) a -> a -> Bool

b) Eq a => a -> a -> Bool

c) Eq a -> a -> a -> Bool

d) Eq a => A -> Bool

=> b

3. Which of the following types is the type of fst ?

a) (a, b) -> a

b) b -> a

c) (a, b) -> b

=> a

4. Which of the following types is the type of (+) ?

a) Num a -> a -> a -> Bool

b) Num a => a -> a -> Bool

c) num a => a -> a -> a

d) Num a => a -> a -> a

e) a -> a -> a

=> d

-}
