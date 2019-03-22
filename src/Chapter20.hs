{-# LANGUAGE NoImplicitPrelude #-}

module Chapter20 where

import Control.Applicative (Applicative)
import Data.Bool           (Bool (False, True), otherwise, (||))
import Data.Eq             (Eq, (==))
import Data.Foldable       (Foldable, fold, foldMap, foldr)
import Data.Int            (Int)
import Data.Maybe          (Maybe (Just, Nothing))
import Data.Monoid         (Monoid, mappend, mempty)
import Data.Ord            (Ord, (<=), (>=))
import GHC.Err             (undefined)
import GHC.Num             (Num, (*), (+))

{-

Exercises: Library Functions

Implement the functions in terms of foldMap or foldr from Foldable,
then try them out with multiple types that have Foldable instances.

1. This and the next one are nicer with foldMap, but foldr is fine too.

sum :: (Foldable t, Num a) => t a -> a

-}

sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

{-

2.

product :: (Foldable t, Num a) => t a -> a

-}

product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

{-

3.

elem :: (Foldable t, Eq a) => a -> t a -> Bool

-}

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem e        = foldr f False
  where f i s = (i == e) || s

{-

4.

minimum :: (Foldable t, Ord a) => t a -> Maybe a

-}

comp :: (a -> a -> Bool) -> a -> Maybe a -> Maybe a
comp _ i Nothing              = Just i
comp f i (Just s) | i `f` s   = Just i
                  | otherwise = Just s

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr (comp (<=)) Nothing

{-

5.

maximum :: (Foldable t, Ord a) => t a -> Maybe a

-}

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr (comp (>=)) Nothing

{-

6.

null :: (Foldable t) => t a -> Bool

-}

null :: (Foldable t) => t a -> Bool
null          = foldr f True
  where f _ _ = False

{-

7.

length :: (Foldable t) => t a -> Int

-}

length :: (Foldable t) => t a -> Int
length        = foldr f 0
  where f _ s = s + 1

{-

8. Some say this is all Foldable amounts to.

toList :: (Foldable t) => t a -> [a]

-}

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

{-

9. Hint: use foldMap

-- | Combine the elements of a structure using a monoid.
fold :: (Foldable t, Monoid m) => t m -> m

-}

-- | Combine the elements of a structure using a monoid.
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldr mappend mempty

{-

10. Define foldMap in terms of foldr.

foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m

-}

foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f     = foldr g mempty
  where g i s = f i `mappend` s

{-


Chapter Exercises

Write Foldable instances for the following datatypes.

1.

data Constant a b = Constant a

-}

data Constant a b = Constant a

instance Foldable (Constant a) where
  fold (Constant _)      = mempty
  foldMap _ (Constant _) = mempty

{-

2.

data Two a b = Two a b

-}

data Two a b = Two a b

instance Foldable (Two a) where
  fold (Two _ x)      = x
  foldMap f (Two _ x) = f x

{-

3.

data Three a b c = Three a b c

-}

data Three a b c = Three a b c

instance Foldable (Three a b) where
  fold (Three _ _ x)      = x
  foldMap f (Three _ _ x) = f x

{-

4.

data Three' a b = Three' a b b

-}

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  fold (Three' _ _ x)      = x
  foldMap f (Three' _ _ x) = f x

{-

5.

data Four' a b = Four' a b b b

-}

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  fold (Four' _ _ _ x)      = x
  foldMap f (Four' _ _ _ x) = f x

{-

Thinking cap time. Write a filter function for Foldable types using foldMap.

filterF :: (Applicative f, Foldable t, Monoid (f a)) =>
  (a -> Bool) -> t a -> f a
filterF = undefined

-}

filterF :: (Applicative f, Foldable t, Monoid (f a)) =>
  (a -> Bool) -> t a -> f a
filterF _ _ = undefined
