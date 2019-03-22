{-# LANGUAGE NoImplicitPrelude #-}

module Chapter6 where

import Data.Bool   (Bool (False, True), (&&))
import Data.Char   (Char)
import Data.Eq     (Eq, (==))
import Data.Int    (Int)
import Data.List   (head, sort)
import Data.Ord    (Ord)
import Data.String (String)
import GHC.Integer (Integer)
import GHC.Num     (Num, fromInteger, (+))
import GHC.Real    (Fractional, RealFrac)
import GHC.Types   (Float)
import System.IO   (IO, print)
import Text.Show   (Show, show)

{-

Intermission: Exercises

Look at the types given for quotRem and
divMod . What do you think those functions do? Test your hypotheses
by playing with them in the REPL. We’ve given you a sample to start
with below:

Prelude> let ones x = snd (divMod x 10)

=> `qoutRem` is both `quot` and `rem` in one
=> `quot` is integer division towards zero
=> `rem` is integer remainder

=> `divMod` is both `div` and `mod` in one
=> `div` is integer division towards negative infinity
=> `mod` is integer modulus

=> it differs if negative numbers are passed to x


Intermission: Exercises

Next, take a look at the following code examples and try to decide
if they will work, what result they will return if they do, and why or
why not (be sure, as always, to test them in your REPL once you have
decided on your answer):

1. max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])
=> will work
=> 5
=> there is an instance of Ord for Int

2. compare (3 * 4) (3 * 5)
=> will work
=> LT
=> there is an instance of Ord for Int

3. compare "Julie" True
=> will not work
=> "Julie" and True are two different types

4. (5 + 3) > (3 + 6)
=> will work
=> False
=> there is an instance of Ord for Int


Intermission: Exercises

Write the Eq instance for the datatype provided.

1. It’s not a typo, we’re just being cute with the name.
data TisAnInteger = TisAn Integer

-}

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
         TisAn i == TisAn i' = i == i'

{-

2. data TwoIntegers = Two Integer Integer

-}

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
         Two x y == Two x' y' = x == x' && y == y'

{-

3. data StringOrInt = TisAnInt Int
                    | TisAString String

-}

data StringOrInt = TisAnInt Int
                 | TisAString String

instance Eq StringOrInt where
         TisAnInt x == TisAnInt x'     = x == x'
         TisAString s == TisAString s' = s == s'
         TisAnInt _ == TisAString _    = False
         TisAString _ == TisAnInt _    = False

{-

4. data Pair a = Pair a a

-}

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
         Pair x y == Pair x' y' = x == x' && y == y' && x == y' && y == x'

{-

5. data Tuple a b = Tuple a b

-}

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
         Tuple x y == Tuple x' y' = x == x' && y == y'

{-

6. data Which a = ThisOne a
                | ThatOne a

-}

data Which a = ThisOne a
             | ThatOne a

instance Eq a => Eq (Which a) where
         ThisOne x == ThisOne x' = x == x'
         ThatOne x == ThatOne x' = x == x'
         ThisOne x == ThatOne x' = x == x'
         ThatOne x == ThisOne x' = x == x'

{-

7. data EitherOr a b = Hello a
                     | Goodbye b

-}

data EitherOr a b = Hello a
                  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
         Hello x == Hello x'     = x == x'
         Goodbye x == Goodbye x' = x == x'
         Hello _ == Goodbye _    = False
         Goodbye _ == Hello _    = False


{-

Chapter Exercises

Multiple choice

1. The Eq class
a) includes all types in Haskell
b) is the same as the Ord class
c) makes equality tests possible
d) only includes numeric types
=> c

2. The typeclass Ord
a) allows any two values to be compared
b) is a subclass of Eq
c) is a superclass of Eq
d) has no instance for Bool
=> b

3. Suppose the typeclass Ord has an operator > . What is the type
of > ?
a) Ord a => a    -> a -> Bool
b) Ord a => Int  -> Bool
c) Ord a => a    -> Char
d) Ord a => Char -> [Char]
=> a

4. In x = divMod 16 12
a) the type of x is Integer
b) the value of x is undecidable
c) the type of x is a tuple
d) x is equal to 12 / 16
=> c

5. The typeclass Integral includes
a) Int and Integer numbers
b) integral, real, and fractional numbers
c) Schrodinger’s cat
d) only positive numbers
=> a

Does it typecheck?

For this section of exercises, you’ll be practicing looking for type and
typeclass errors.

For example, printIt will not work because functions like x have no
instance of Show, the typeclass that lets you convert things to Strings
(usually for printing):

x:: Int -> Int
x blah = blah + 20

printIt :: IO ()
printIt = putStrLn (show x)

Here’s the type error you get if you try to load the code:

No instance for (Show (Int -> Int)) arising
from a use of ‘show’

In the first argument of ‘putStrLn’, namely ‘(show x)’
In the expression: putStrLn (show x)
In an equation for ‘printIt’: printIt = putStrLn (show x)

It’s saying it can’t find an implementation of the typeclass Show for
the type Int -> Int , which makes sense. Nothing with the function
type constructor (->) has an instance of Show 5 by default in Haskell.

Examine the following code and decide whether it will typecheck.
Then load it in GHCi and see if you were correct. If it doesn’t type-
check, try to match the type error against your understanding of why
it didn’t work. If you can, fix the error and re-run the code.

1. Does the following code typecheck? If not, why not?

data Person = Person Bool

printPerson :: Person -> IO ()

printPerson person = putStrLn (show person)

=> doesn't typecheck
=> Person has no instance of Show

-}

data Person = Person Bool

instance Show Person where
         show (Person True)  = "Person True"
         show (Person False) = "Person False"

printPerson :: Person -> IO ()
printPerson = print

{-

2. Does the following typecheck? If not, why not?

data Mood = Blah
          | Woot deriving Show

settleDown x = if x == Woot
                 then Blah
                 else x

=> doesn't typecheck
=> Mood has no instance of Eq

-}

data Mood = Blah
          | Woot deriving Show

instance Eq Mood where
         Blah == Blah = True
         Woot == Woot = True
         _ == _       = False

settleDown :: Mood -> Mood
settleDown x = if x == Woot
                 then Blah
                 else x

{-

3. If you were able to get settleDown to typecheck:
a) What values are acceptable inputs to that function?
=> Mood

b) What will happen if you try to run settleDown 9 ? Why?
=> error
=> 9 is a different type compared to Mood

c) What will happen if you try to run Blah > Woot ? Why?
=> error
=> Mood has no instance of Ord

4. Does the following typecheck? If not, why not?

type Subject = String
type Verb    = String
type Object  = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

=> will typecheck

-}

type Subject = String
type Verb    = String
type Object  = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 :: Object -> Sentence
s1 = Sentence "dogs" "drool"

s2 :: Sentence
s2 = Sentence "Julie" "loves" "dogs"

{-

Given a datatype declaration, what can we do?

Given the following datatype definitions:

-}

data Rocks = Rocks String deriving (Eq, Show)

data Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)

{-

Which of the following will typecheck? For the ones that don’t type-
check, why don’t they?

1.
phew = Papu "chases" True
=> will not typecheck
=> Papu has type Rocks and Yeah in its data constructor

2.
truth = Papu (Rocks "chomskydoz") (Yeah True)
=> will typecheck

3.
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'
=> will typecheck

4.
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'
=> will not typecheck
=> Papu has no instance of Ord


Match the types

We’re going to give you two types and their implementations. Then
we’re going to ask you if you can substitute the second type for the
first. You can test this by typing the first declaration and its type into
a file and editing in the new one, loading to see if it fails. Don’t just
guess, test all your answers!

1. For the following definition.
a)
i :: Num a => a
i = 1

b) Try replacing the type signature with the following:
i :: a
After you’ve formulated your own answer, then tested that
answer and believe you understand why you were right or
wrong, make sure to use GHCi to check what type GHC infers
for the definitions we provide without a type assigned. For
example, for this one, you’d type in:
Prelude> let i = 1
Prelude> :t i
-- Result elided intentionally.

-}

ii   :: Num a => a
--ii :: a
ii = 1

-- cant replace with i :: a because 1 is type constrained to Num

{-

2.
a)
f :: Float
f = 1.0

b)
f :: Num a => a

-}

f   :: Float
--f :: Num a => a
f = 1.0

-- cant replace with f :: Num a => a because 1.0 is type constrained to
-- Fractional

{-

3.
a)
f :: Float
f = 1.0

b)
f :: Fractional a => a

-}

--ff :: Float
ff   :: Fractional a => a
ff = 1.0

-- will typecheck on both

{-

4. Hint for the following: type :info
a)
f :: Float
f = 1.0

b)
f :: RealFrac a => a

-}

--fff :: Float
fff   :: RealFrac a => a
fff = 1.0

-- will typecheck on both

{-

5.
a)
freud :: a -> a
freud x = x

b)
freud :: Ord a => a -> a

-}

freud   :: a          -> a
--freud :: Ord a => a -> a
freud x = x

-- will typecheck on both

{-

6.
a)
freud' :: a -> a
freud' x = x

b)
freud' :: Int -> Int

-}

--freud' :: a   -> a
freud'   :: Int -> Int
freud' x = x

-- will typecheck on both

{-

7.
a)
myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX

b)
sigmund :: a -> a

-}

myX :: Int
myX = 1

sigmund   :: Int -> Int
--sigmund :: a   -> a
sigmund _ = myX

-- will not typecheck on sigmund :: a -> a because myX is already of type Int

{-

8.
a)
myX = 1 :: Int

sigmund' :: Int -> Int
sigmund' x = myX

b)
sigmund' :: Num a => a -> a

-}

myX' :: Int
myX' = 1

sigmund'   :: Int        -> Int
--sigmund' :: Num a => a -> a
sigmund' _ = myX'

-- will not typecheck on sigmund :: Num a => a -> a because myX is already of
-- type Int

{-

9.
a) You’ll need to import sort from Data.List .
jung :: Ord a => [a] -> a
jung xs = head (sort xs)

b)
jung :: [Int] -> Int

-}

--jung :: Ord a => [a] -> a
jung   :: [Int]        -> Int
jung xs = head (sort xs)

-- will typecheck on both

{-

10.
a)
young :: [Char] -> Char
young xs = head (sort xs)

b)
young :: Ord a => [a] -> a

-}

--young :: String       -> Char
young   :: Ord a => [a] -> a
young xs = head (sort xs)

-- will typecheck

{-

11.
a)
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

b)
signifier :: Ord a => [a] -> a

-}

mySort :: String -> String
mySort = sort

signifier   :: String       -> Char
--signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)

-- will not typecheck because mySort has type String -> String


{-

Type-Kwon-Do

Round Two! Same rules apply — you’re trying to fill in terms (code)
which’ll fit the type. The idea with these exercises is that you’ll derive
the implementation from the type information. You’ll probably need
to use stuff from Prelude.

1.
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk = ???

2.
-- Hint: use some arithmetic operation to
-- combine values of type 'b'. Pick one.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith = ???

-}

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk fu x y = fu x == y

arith :: Num b => (a -> b) -> Integer -> a -> b
arith fu i x = fu x + fromInteger i
