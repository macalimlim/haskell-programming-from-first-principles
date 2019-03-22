{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude    #-}

module Chapter12 where

import Control.Monad (return)
import Data.Bool     (Bool (False, True), not, otherwise)
import Data.Either   (Either (Left, Right))
import Data.Eq       (Eq, (==))
import Data.Function (($), (.))
import Data.List     (concatMap, elem, filter, foldr, init, length, words, (++))
import Data.Maybe    (Maybe (Just, Nothing))
import Data.Ord      (Ord, (>), (>=))
import Data.String   (String)
import GHC.Integer   (Integer)
import GHC.Num       ((+), (-))
import GHC.Real      (fromIntegral)
import Text.Show     (Show)

{-

Chapter Exercises

Determine the kinds
1. Given

id :: a -> a

What is the kind of a?
=> *

2. r :: a -> f a

What are the kinds of a and f?
=> *
=> * -> *


String processing

Because this is the kind of thing linguists ahem enjoy doing in their
spare time.

1. Write a recursive function that takes a text/string, breaks it into
words and replaces each instance of “the” with “a”. It’s intended
only to replace exactly the word “the”.

-- example GHCi session above the functions
-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"

notThe :: String -> Maybe String
notThe = undefined

-- >>> replaceThe "the cow loves us"
-- "a cow loves us"

replaceThe :: String -> String
replaceThe = undefined

-}

notThe :: String -> Maybe String
notThe ['t', 'h', 'e'] = Nothing
notThe s               = Just s

notThe' :: String      -> String
notThe' s = case notThe s of
              Nothing  -> "a "
              (Just t) -> t ++ " "

replaceThe :: String -> String
replaceThe = init . concatMap notThe' . words

{-

2. Write a recursive function that takes a text/string, breaks it into
words, and counts the number of instances of ”the” followed by
a vowel-initial word.

-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> countTheBeforeVowel "the evil cow"
-- 1

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = undefined

-}

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel                = f . words
  where f ("the" : ('a' : _) : xs) = 1 + f xs
        f ("the" : ('e' : _) : xs) = 1 + f xs
        f ("the" : ('i' : _) : xs) = 1 + f xs
        f ("the" : ('o' : _) : xs) = 1 + f xs
        f ("the" : ('u' : _) : xs) = 1 + f xs
        f ("the" : ('A' : _) : xs) = 1 + f xs
        f ("the" : ('E' : _) : xs) = 1 + f xs
        f ("the" : ('I' : _) : xs) = 1 + f xs
        f ("the" : ('O' : _) : xs) = 1 + f xs
        f ("the" : ('U' : _) : xs) = 1 + f xs
        f (_ : _ : xs)             = f xs
        f (_ : _)                  = 0
        f []                       = 0

{-

3. Return the number of letters that are vowels in a word.
Hint: it’s helpful to break this into steps. Add any helper func-
tions necessary to achieve your objectives.

a) Test for vowelhood
b) Return the vowels of a string
c) Count the number of elements returned

-- >>> countVowels "the cow"
-- 2
-- >>> countVowels "Mikolajczak"
-- 4

countVowels :: String -> Integer
countVowels = undefined

-}

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter (`elem` "aeiouAEIOU")

{-

Validate the word

Use the Maybe type to write a function that counts the number of
vowels in a string and the number of consonants. If the number
of vowels exceeds the number of consonants, the function returns
Nothing. In many human languages, vowels rarely exceed the number
of consonants so when they do, it indicates the input isn’t a real word
(that is, a valid input to your dataset):

newtype Word' = Word' String
              deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord = undefined

-}

newtype Word' = Word' String
              deriving (Eq, Show)

vowels :: String
vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s                                  = eval
  where isVowel                           = (`elem` vowels)
        isConsonant                       = not . isVowel
        len                               = fromIntegral . length
        lenVowels                         = len $ filter isVowel s
        lenConsonants                     = len $ filter isConsonant s
        eval | lenVowels >= lenConsonants = Nothing
             | otherwise                  = Just $ Word' s

{-

It’s only Natural

You’ll be presented with a datatype to represent the natural numbers.
The only values representable with the naturals are whole numbers
from zero to infinity. Your task will be to implement functions to
convert Naturals to Integers and Integers to Naturals. The conversion
from Naturals to Integers won’t return Maybe because Integer is a strict
superset of Natural. Any Natural can be represented by an Integer,
but the same is not true of any Integer. Negative numbers are not
valid natural numbers.

-- As natural as any competitive bodybuilder

data Nat = Zero
         | Succ Nat
         deriving (Eq, Show)

-- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Succ (Succ Zero))
-- 2

natToInteger :: Nat -> Integer
natToInteger = undefined

-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing

integerToNat :: Integer -> Maybe Nat
integerToNat = undefined

-}

data Nat = Zero
         | Succ Nat
         deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat 0             = Just Zero
integerToNat x | x > 0     = Just $ go x
               | otherwise = Nothing
  where go 0               = Zero
        go n | n > 0       = Succ $ go $ n - 1
             | otherwise   = Zero

{-

Small library for Maybe

Write the following functions. This may take some time.

1. Simple boolean checks for Maybe values.

-- >>> isJust (Just 1)
-- True
-- >>> isJust Nothing
-- False

isJust :: Maybe a -> Bool

-- >>> isNothing (Just 1)
-- False
-- >>> isNothing Nothing
-- True

isNothing :: Maybe a -> Bool

-}

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

{-

2. The following is the Maybe catamorphism. You can turn a Maybe
value into anything else with this.

-- >>> mayybee 0 (+1) Nothing
-- 0
-- >>> mayybee 0 (+1) (Just 1)
-- 2

mayybee :: b -> (a -> b) -> Maybe a -> b

-}

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee acc f  = foldr go acc
  where go i _ = f i

{-

3. In case you just want to provide a fallback value.

-- >>> fromMaybe 0 Nothing
-- 0
-- >>> fromMaybe 0 (Just 1)
-- 1

fromMaybe :: a -> Maybe a -> a

-- Try writing it in terms of the maybe catamorphism

-}

fromMaybe :: a -> Maybe a -> a
fromMaybe v Nothing  = v
fromMaybe _ (Just v) = v

{-

4. Converting between List and Maybe.

-- >>> listToMaybe [1, 2, 3]
-- Just 1
-- >>> listToMaybe []
-- Nothing

listToMaybe :: [a] -> Maybe a

-- >>> maybeToList (Just 1)
-- [1]
-- >>> maybeToList Nothing
-- []

maybeToList :: Maybe a -> [a]

-}

listToMaybe :: [a] -> Maybe a
listToMaybe (x : _) = Just x
listToMaybe []      = Nothing

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing  = []

{-

5. For when we just want to drop the Nothing values from our list.

-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1, 2]
-- >>> catMaybes [Nothing, Nothing, Nothing]
-- []

catMaybes :: [Maybe a] -> [a]

-}

catMaybes :: [Maybe a] -> [a]
catMaybes           = concatMap go
  where go (Just x) = [x]
        go Nothing  = []

{-

6. You’ll see this called “sequence” later.

-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing

flipMaybe :: [Maybe a] -> Maybe [a]

-}

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe ms | length ms == length ns = Just ns
             | otherwise              = Nothing
  where ns                            = catMaybes ms

{-

Small library for Either

Write each of the following functions. If more than one possible
unique function exists for the type, use common sense to determine
what it should do.

1. Try to eventually arrive at a solution that uses foldr, even if
earlier versions don’t use foldr.

lefts' :: [Either a b] -> [a]

-}

lefts' :: [Either a b] -> [a]
lefts'                = foldr go []
  where go (Left i) s = i : s
        go _ s        = s

{-

2. Same as the last one. Use foldr eventually.

rights' :: [Either a b] -> [b]

-}

rights' :: [Either a b] -> [b]
rights'                = foldr go []
  where go (Right i) s = i : s
        go _ s         = s

{-

3. partitionEithers' :: [Either a b] -> ([a], [b])

-}

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers'             = foldr go ([], [])
  where go (Left i) (ls, rs)  = (i : ls, rs)
        go (Right i) (ls, rs) = (ls, i : rs)

{-

4. eitherMaybe' :: (b -> c) -> Either a b -> Maybe c

-}

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = return $ f x
eitherMaybe' _ _         = Nothing

{-

5. This is a general catamorphism for Either values.

either' :: (a -> c) -> (b -> c) -> Either a b -> c

-}

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ f (Right x) = f x

{-

6. Same as before, but use the either' function you just wrote.

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c

-}

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' = eitherMaybe'

{-

Write your own iterate and unfoldr

1. Write the function myIterate using direct recursion. Compare
the behavior with the built-in iterate to gauge correctness. Do
not look at the source or any examples of iterate so that you
are forced to do this yourself.

myIterate :: (a -> a) -> a -> [a]
myIterate = undefined

-}

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

{-

2. Write the function myUnfoldr using direct recursion. Compare
with the built-in unfoldr to check your implementation. Again,
don’t look at implementations of unfoldr so that you figure it
out yourself.

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr = undefined

-}

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x              = go (f x)
  where go (Just (x', y')) = x' : myUnfoldr f y'
        go Nothing         = []

{-

3. Rewrite myIterate into betterIterate using myUnfoldr. A hint —
we used unfoldr to produce the same results as iterate earlier.
Do this with different functions and see if you can abstract the
structure out.

-- It helps to have the types in front of you
-- myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr ...?

Remember, your betterIterate should have the same results as
iterate.

Prelude> take 10 $ iterate (+1) 0
[0,1,2,3,4,5,6,7,8,9]

Prelude> take 10 $ betterIterate (+1) 0
[0,1,2,3,4,5,6,7,8,9]

-}

betterIterate :: Eq a => (a -> a) -> a -> [a]
betterIterate f = myUnfoldr g
  where g x     = Just (x, f x)

{-

Finally something other than a list!

Given the BinaryTree from last chapter, complete the following exer-
cises. Here’s that datatype again:

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

-}

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

{-

1. Write unfold for BinaryTree.

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold = undefined

-}

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x                  = go (f x)
  where go (Just (l, v, r)) = Node (unfold f l) v (unfold f r)
        go Nothing          = Leaf

{-

2. Make a tree builder.

Using the unfold function you’ve just made for BinaryTree, write
the following function:

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = undefined

-}

treeBuild :: Integer -> BinaryTree Integer
treeBuild n              = unfold go 0
  where go x | x == n    = Nothing
             | otherwise = Just (x + 1, x, x + 1)
