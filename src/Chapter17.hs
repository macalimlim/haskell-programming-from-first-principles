{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude    #-}

module Chapter17 where

import Control.Applicative      (Applicative, liftA3, pure, (<*>))
import Control.Monad            (return)
import Data.Bool                (otherwise)
import Data.Eq                  (Eq)
import Data.Function            (const, ($))
import Data.Functor             (Functor, fmap, (<$>))
import Data.Int                 (Int)
import Data.Ix                  (range)
import Data.List                (elemIndex, lookup, sum, zip)
import Data.Maybe               (Maybe (Just))
import Data.Monoid              (Monoid, mappend, mempty)
import Data.Ord                 (Ord, max, (>))
import Data.Semigroup           (Semigroup, (<>))
import Data.String              (String)
import GHC.Err                  (undefined)
import GHC.Integer              (Integer)
import GHC.Num                  ((+), (-))
import System.IO                (IO)
import Test.QuickCheck          (Arbitrary, arbitrary, oneof)
import Test.QuickCheck.Checkers (EqProp, eq, quickBatch, (=-=))
import Test.QuickCheck.Classes  (applicative)
import Text.Show                (Show)

{-

Exercises: Lookups

In the following exercises you will need to use the following terms
to make the expressions type-check:

1. pure

2. (<$>)
-- or fmap

3. (<*>)

Make the following expressions type-check.

1.

added :: Maybe Integer
added = (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-}

added :: Maybe Integer
added = pure (+ 3) <*> lookup 3 (zip [1, 2, 3] [4, 5, 6])

{-

2.

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) y z

-}

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

{-

3.

import Data.List (elemIndex)

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y :: Maybe Int
y = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' x y

-}

x' :: Maybe Int
x' = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x' <*> y'

{-

4.

xs = [1, 2, 3]

ys = [4, 5, 6]

x :: Maybe Integer
x = lookup 3 $ zip xs ys

y :: Maybe Integer
y = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum $ (,) x y

-}

xs' :: [Integer]
xs' = [1, 2, 3]

ys' :: [Integer]
ys' = [4, 5, 6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs' ys'

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs' ys'

summed :: Maybe Integer
summed = sum <$> (range <$> ((,) <$> x'' <*> y''))

{-

Exercise: Identity Instance

Write an Applicative instance for Identity.

newtype Identity a = Identity a
                   deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap = undefined

instance Applicative Identity where
  pure  = undefined
  (<*>) = undefined

-}

newtype Identity a = Identity a
                   deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure                      = Identity
  Identity f <*> Identity x = pure $ f x

{-

Exercise: Constant Instance

Write an Applicative instance for Constant.

newtype Constant a b = Constant { getConstant :: a }
                     deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap = undefined

instance Monoid a => Applicative (Constant a) where
  pure  = undefined
  (<*>) = undefined

-}

newtype Constant a b = Constant { getConstant :: a }
                     deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _                    = Constant mempty
  Constant _ <*> Constant x = Constant x

{-

Exercise: Fixer Upper

Given the function and values provided, use (<$>) from Functor,
(<*>) and pure from the Applicative typeclass to fill in missing bits of
the broken code to make it work.

1.

const <$> Just "Hello" <*> "World"

-}

ex1 :: Maybe String
ex1 =  const <$> Just "Hello" <*> pure "World"

{-

2.

(,,,) Just 90 <*> Just 10 Just "Tierness" [1, 2, 3]

-}

ex2 :: Maybe (Integer, Integer, String, [Integer])
ex2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

{-

List Applicative Exercise

Implement the List Applicative. Writing a minimally complete Applicative
instance calls for writing the definitions of both pure and
<*>. We’re going to provide a hint as well. Use the checkers library to
validate your Applicative instance.

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

Remember what you wrote for the List Functor:

instance Functor List where
  fmap = undefined

-}

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

--testApplicative :: TestBatch
--testApplicative = quickBatch $ applicative $ [("", "", 1)]

{-

Writing the List Applicative is similar.

instance Applicative List where
  pure  = undefined
  (<*>) = undefined

-}

instance Applicative List where
  pure x             = Cons x Nil
  Nil <*> _          = Nil
  _ <*> Nil          = Nil
  (Cons f fs) <*> xs = fmap f xs `append` (fs <*> xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do x <- arbitrary
                 return $ Cons x Nil

instance Eq a => EqProp (List a) where
  (=-=) = eq

testApplicative :: IO ()
testApplicative = quickBatch $ applicative $ Cons ("mike", "aya", "kc") Nil

{-

Expected result:

Prelude> let functions = Cons (+1) (Cons (*2) Nil)
Prelude> let values    = Cons 1 (Cons 2 Nil)
Prelude> functions <*> values
Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

In case you get stuck, use the following functions and hints.

-}

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

{-

-- write this one in terms of concat' and fmap

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = undefined

-}

flatMap :: (a -> List b) -> List a -> List b
flatMap _ Nil         = Nil
flatMap f (Cons x xs) = concat' $ Cons (f x) (fmap f xs)

{-

Use the above and try using flatMap and fmap without explicitly
pattern-matching on Cons cells. You’ll still need to handle the Nil
cases.

flatMap is less strange than it would initially seem. It’s basically
“fmap, then smush”.

Prelude> fmap (\x -> [x, 9]) [1, 2, 3]
[[1,9],[2,9],[3,9]]

Prelude> flatMap (\x -> [x, 9]) [1, 2, 3]
[1,9,2,9,3,9]

Applicative instances, unlike Functors, are not guaranteed to have
a unique implementation for a given datatype.

-}

{-

ZipList Applicative Exercise

Implement the ZipList Applicative. Use the checkers library to vali-
date your Applicative instance. We’re going to provide the EqProp
instance and explain the weirdness in a moment.

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

take' :: Int -> List a -> List a
take' = undefined

instance Functor List where
  fmap = undefined

instance Applicative List where
  pure  = undefined
  (<*>) = undefined

newtype ZipList' a = ZipList' (List a)
                   deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys   = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure  = undefined
  (<*>) = undefined


The idea is to align a list of functions with a list of values and
apply the first function to the first value and so on. The instance
should work with infinite lists. Some examples:

Prelude> let z  = ZipList' [(+9), (*2), (+8)]
Prelude> let z' = ZipList' [1..3]
Prelude> z <*> z'
ZipList' [10,4,11]

Prelude> let z' = ZipList' (repeat 1)
Prelude> z <*> z'
ZipList' [10,2,9]

Note that the second z' was an infinite list. Check Prelude for
functions that can give you what you need. One starts with the letter
z, the other with the letter r. You’re looking for inspiration from
these functions, not to be able to directly reuse them as you’re using
a custom List type and not the provided Prelude list type.

-}

data List' a = Nil'
             | Cons' a (List' a)
             deriving (Eq, Show)

take' :: Int -> List' a -> List' a
take' 0 (Cons' _ _)              = Nil'
take' _ Nil'                     = Nil'
take' n (Cons' x xs) | n > 0     = Cons' x $ take' (n - 1) xs
                     | otherwise = Nil'

instance Functor List' where
  fmap _ Nil'         = Nil'
  fmap f (Cons' x xs) = Cons' (f x) (fmap f xs)

instance Semigroup (List' a) where
  xs <> Nil'         = xs
  Nil' <> xs         = xs
  (Cons' x xs) <> ys = Cons' x $ xs <> ys

instance Monoid (List' a) where
  mempty  = Nil'
  mappend = (<>)

instance Applicative List' where
  pure x              = Cons' x Nil'
  Nil' <*> _          = Nil'
  _ <*> Nil'          = Nil'
  (Cons' f fs) <*> xs = (f <$> xs) <> (fs <*> xs)

newtype ZipList' a = ZipList' (List' a)
                   deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys    = xs'' `eq` ys''
    where xs'' = let (ZipList' l) = xs
                 in take' 3000 l
          ys'' = let (ZipList' l) = ys
                 in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

zipWith :: (a -> b -> c) -> List' a -> List' b -> List' c
zipWith _ _ Nil'                     = Nil'
zipWith _ Nil' _                     = Nil'
zipWith f (Cons' x xs) (Cons' z' zs) = Cons' (f x z') $ zipWith f xs zs

instance Applicative ZipList' where
  pure x                      = ZipList' $ pure x
  ZipList' fs <*> ZipList' xs = ZipList' $ zipWith ($) fs xs

instance Arbitrary a => Arbitrary (List' a) where
  arbitrary = do x <- arbitrary
                 return $ Cons' x Nil'

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do xs <- arbitrary
                 return $ ZipList' xs

testApplicative2 :: IO ()
testApplicative2 = quickBatch $ applicative $ ZipList' $
  Cons' ("mike", "aya", "kc") Nil'

{-

Exercise: Variations on Either

Validation has the same representation as Either, but it can be different.
The Functor will behave the same, but the Applicative will be
different. See above for an idea of how Validation should behave.
Use the checkers library.

data Validation e a = Failure e
                    | Success a
                    deriving (Eq, Show)
-- same as Either

instance Functor (Validation e) where
  fmap = undefined
  -- This is different

instance Monoid e => Applicative (Validation e) where
  pure  = undefined
  (<*>) = undefined

-}

data Validation e a = Failure e
                    | Success a
                    deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure x) = Failure x
  fmap f (Success x) = Success $ f x

instance Monoid e => Applicative (Validation e) where
  pure                          = Success
  (Failure x) <*> (Failure z'') = Failure $ x `mappend` z''
  (Failure x) <*> _             = Failure x
  _ <*> (Failure x)             = Failure x
  (Success f) <*> succ          = f <$> succ

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do xs <- arbitrary
                 x  <- arbitrary
                 oneof [return $ Failure xs, return $ Success x]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (Failure a) =-= (Failure b) = a `eq` b
  (Success a) =-= (Success b) = a `eq` b
  a =-= b                     = a `eq` b

testValidation :: IO ()
testValidation   = undefined
--testValidation = quickBatch $ applicative $ Success ("mike", "aya", "kc")

{-

Chapter Exercises

Given a type that has an instance of Applicative, specialize the types
of the methods. Test your specialization in the REPL. One way to
do this is to bind aliases of the typeclass methods to “more concrete”
types that have the type we told you to fill in.

1.
-- Type
[]

-- Methods
pure :: a -> ? a

(<*>) :: ? (a -> b) -> ? a -> ? b

--Answers
pure :: a -> [a]

(<*>) :: [(a -> b)] -> [a] -> [b]

-}

pure' :: a -> [a]
pure' x = [x]

(<**>) :: [(a -> b)] -> [a] -> [b]
[] <**> _        = []
_ <**> []        = []
(f : fs) <**> xs = (f <$> xs) <> (fs <*> xs)

{-

2.
-- Type
IO

-- Methods
pure :: a -> ? a

(<*>) :: ? (a -> b) -> ? a -> ? b

-}

pure'' :: a -> IO a
pure'' = return

(<***>) :: IO (a -> b) -> IO a -> IO b
iof <***> iox = do f <- iof
                   x <- iox
                   return $ f x

{-

3.
-- Type
(,) a

-- Methods
pure :: a -> ? a

(<*>) :: ? (a -> b) -> ? a -> ? b

-}

pure''' :: a -> (a, a)
pure''' x = (x, x)

(<****>) :: (a, (a -> b))  -> (a, a) -> (a, b)
(_, f) <****> (xx, x) = (xx, f x)

{-

4.
-- Type
(->) e

-- Methods
pure :: a -> ? a

(<*>) :: ? (a -> b) -> ? a -> ? b

-}

pure'''' :: a -> (e -> a)
pure''''      = return
  --where f _ = x

(<*****>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
(<*****>) ff gx = do f <- ff
                     x <- gx
                     return $ f x

{-

Write applicative instances for the following datatypes. Confused?
Write out what the type should be. Use the checkers library to validate
the instances.

1. data Pair a = Pair a a deriving Show

-}

data Pair a = Pair a a
            deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure a                      = Pair a a
  (Pair f1 f2) <*> (Pair a b) = Pair (f1 a) (f2 b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do x <- arbitrary
                 return $ Pair x x

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

testPair :: IO ()
testPair = quickBatch $ applicative $ Pair ("mike", "aya", "kc")
  ("mike", "aya", "kc")

{-

2. This should look familiar.
data Two a b = Two a b

-}

data Two a b = Two a b
             deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure x                    = Two mempty x
  (Two a f') <*> (Two a' b) = Two (a `mappend` a') (f' b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

testTwo :: IO ()
testTwo = quickBatch $ applicative $ Two ("mike", "aya", "kc")
  ("mike", "aya", "kc")

{-

3. data Three a b c = Three a b c

-}

data Three a b c = Three a b c
                 deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x                            = Three mempty mempty x
  (Three a b f) <*> (Three a' b' c) = Three (a `mappend` a') (b `mappend` b') (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 c <- arbitrary
                 return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

testThree :: IO ()
testThree = quickBatch $ applicative $ Three ("mike", "aya", "kc")
  ("mike", "aya", "kc") ("mike", "aya", "kc")

{-

4. data Three' a b = Three' a b b

-}

data Three' a b = Three' a b b
                deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
  pure x                              = Three' mempty x x
  (Three' a f f') <*> (Three' a' b c) = Three' (a `mappend` a') (f b) (f' c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 return $ Three' a b b

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

testThree' :: IO ()
testThree' = quickBatch $ applicative $ Three' ("mike", "aya", "kc")
  ("mike", "aya", "kc") ("mike", "aya", "kc")

{-

5. data Four a b c d = Four a b c d

-}

data Four a b c d = Four a b c d
                  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x                                = Four mempty mempty mempty x
  (Four a b c f) <*> (Four a' b' c' d') = Four (a `mappend` a') (b `mappend` b')
    (c `mappend` c') (f d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 c <- arbitrary
                 d <- arbitrary
                 return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

testFour :: IO ()
testFour = quickBatch $ applicative $ Four ("mike", "aya", "kc")
  ("mike", "aya", "kc") ("mike", "aya", "kc") ("mike", "aya", "kc")

{-

6. data Four' a b = Four' a a a b

-}

data Four' a b = Four' a a a b
               deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance Monoid a => Applicative (Four' a) where
  pure                                    = Four' mempty mempty mempty
  (Four' a b c f) <*> (Four' a' b' c' d') = Four' (a `mappend` a') (b `mappend` b')
    (c `mappend` c') (f d')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 return $ Four' a a a b

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

testFour' :: IO ()
testFour' = quickBatch $ applicative $ Four' ("mike", "aya", "kc")
  ("mike", "aya", "kc") ("mike", "aya", "kc") ("mike", "aya", "kc")

{-

Combinations

Remember the vowels and stops chapter exercise in folds? Write the
function to generate the possible combinations of three input lists
using liftA3 from Control.Applicative.

import Control.Applicative (liftA3)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = undefined

-}

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos xs ys zs = liftA3 (,,) xs ys zs
