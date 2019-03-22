{-# LANGUAGE NoImplicitPrelude #-}

module Chapter21 where

import Control.Monad            (return)
import Data.Eq                  (Eq)
import Data.Foldable            (Foldable, foldMap, foldr)
import Data.Function            (($), (.))
import Data.Functor             (Functor, fmap)
import Data.Int                 (Int)
import Data.Monoid              (mappend, mempty)
import Data.Ord                 (Ord)
import Data.Traversable         (Traversable, sequenceA, traverse)
import GHC.Err                  (undefined)
import System.IO                (IO)
import Test.QuickCheck          (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers (EqProp, eq, quickBatch, (=-=))
import Test.QuickCheck.Classes  (traversable)
import Text.Show                (Show)

{-

Chapter Exercises

Traversable instances

Write a Traversable instance for the datatype provided, filling in any
required superclasses. Use QuickCheck to validate your instances.

Identity

Write a Traversable instance for Identity.

newtype Identity a = Identity a
                   deriving (Eq, Ord, Show)

instance Traversable Identity where
traverse = undefined

-}

newtype Identity a = Identity a
                   deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
  foldMap f (Identity x) = f x
  foldr f s (Identity x) = f x s

instance Traversable Identity where
  traverse = (sequenceA .) . fmap

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do x <- arbitrary
                 return $ Identity x

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

testIdentityTraversable    :: IO ()
testIdentityTraversable = quickBatch $ traversable
  (Identity (1, 2, [3, 4]) :: Identity (Int, Int, [Int]))

{-

Constant

newtype Constant a b = Constant { getConstant :: a }

-}

newtype Constant a b = Constant { getConstant :: a }
                     deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ _ = mempty
  foldr _ s _ = s

instance Traversable (Constant a) where
  traverse = (sequenceA .) . fmap

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = do x <- arbitrary
                 return $ Constant x

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

testConstantTraversable    :: IO ()
testConstantTraversable = quickBatch $ traversable
  (Constant (1, 2, [3, 4]) :: Constant (Int, Int, [Int]) (Int, Int, [Int]))

{-

Maybe

data Optional a = Nada
                | Yep a

-}

data Optional a = Nada
                | Yep a
                deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep x) = Yep $ f x

instance Foldable Optional where
  foldMap _ Nada    = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse = (sequenceA .) . fmap

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do x <- arbitrary
                 return $ Yep x

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

testOptionalTraversable :: IO ()
testOptionalTraversable = quickBatch $ traversable
  (Yep (1, 2, [3, 4])   :: Optional (Int, Int, [Int]))

{-

List

data List a = Nil
            | Cons a (List a)

-}

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil         = mempty
  foldMap f (Cons x xs) = f x `mappend` foldMap f xs

instance Traversable List where
  traverse = (sequenceA .) . fmap

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do x  <- arbitrary
                 xs <- arbitrary
                 elements [Cons x xs, Nil]

instance Eq a => EqProp (List a) where
  (=-=) = eq

testListTraversable        :: IO ()
testListTraversable = quickBatch $ traversable
  (Cons (1, 2, [3, 4]) Nil :: List (Int, Int, [Int]))

{-

Three

data Three a b c = Three a b c

-}

data Three a b c = Three a b c
                 deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
  foldMap f (Three _ _ x) = f x
  foldr f s (Three _ _ x) = f x s

instance Traversable (Three a b) where
  traverse = (sequenceA .) . fmap

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do x <- arbitrary
                 y <- arbitrary
                 z <- arbitrary
                 return $ Three x y z

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

testThreeTraversable :: IO ()
testThreeTraversable = quickBatch $ traversable
  (Three (1, 2, [3, 4]) (1, 2, [3, 4]) (1, 2, [3, 4])
                     :: Three (Int, Int, [Int]) (Int, Int, [Int])
                     (Int, Int, [Int]))

{-

Three’

data Three' a b = Three' a b b

-}

data Three' a b = Three' a b b
                deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y y') = Three' x (f y) (f y')

instance Foldable (Three' a) where
  foldMap f (Three' _ _ x) = f x
  foldr f s (Three' _ _ x) = f x s

instance Traversable (Three' a) where
  traverse = (sequenceA .) . fmap

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do x  <- arbitrary
                 y  <- arbitrary
                 y' <- arbitrary
                 return $ Three' x y y'

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

testThree'Traversable :: IO ()
testThree'Traversable = quickBatch $ traversable
  (Three' (1, 2, [3, 4]) (1, 2, [3, 4]) (1, 2, [3, 4])
                      :: Three' (Int, Int, [Int]) (Int, Int, [Int]))

{-

S

This’ll suck.

data S n a = S (n a) a

-- to make it easier, we'll give you the constraints.

instance Traversable n => Traversable (S n) where
  traverse = undefined

-}

data S n a = S (n a) a

--instance Functor n => Functor (S n) where
--  fmap f (S f' x) = S f' (f x)

--instance Traversable n => Traversable (S n) where
--  traverse = undefined

und :: a
und = undefined
