{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude    #-}

module Chapter18 where

import Control.Applicative      (Applicative, pure, (<*>))
import Control.Monad            (Monad, join, mapM_, return, (>>=))
import Data.Eq                  (Eq, (==))
import Data.Foldable            (Foldable, foldMap, foldl, foldr)
import Data.Function            (id, ($), (.))
import Data.Functor             (Functor, fmap)
import Data.Int                 (Int)
import Data.Monoid              (Monoid, mappend, mempty)
import Data.Semigroup           (Semigroup, (<>))
import Data.String              (String)
import GHC.Err                  (undefined)
import System.IO                (IO)
import Test.QuickCheck          (Arbitrary, Gen, Property, arbitrary, elements,
                                 forAll, quickCheck)
import Test.QuickCheck.Checkers (EqProp, TestBatch, eq, quickBatch, (=-=))
import Test.QuickCheck.Classes  (applicative, functor, monad)
import Text.Show                (Show)

{-

The answer is the exercise Write bind in terms of fmap and join.
Fear is the mind-killer, friend. You can do it.

-- keep in mind this is (>>=) flipped
bind :: Monad m => (a -> m b) -> m a -> m b
bind = undefined

-}

bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ fmap f m

{-

Short Exercise: Either Monad

Implement the Either Monad.

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where
  fmap = undefined

instance Applicative (Sum a) where
  pure  = undefined
  (<*>) = undefined

instance Monad (Sum a) where
  return = pure
  (>>=)  = undefined

-}

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second x) = Second $ f x

instance Applicative (Sum a) where
  pure               = Second
  (First x) <*> _    = First x
  (Second f) <*> sec = fmap f sec

instance Monad (Sum a) where
  return           = pure
  (First x) >>= _  = First x
  (Second x) >>= f = f x

{-

Chapter Exercises

Write Monad instances for the following types. Use the QuickCheck
properties we showed you to validate your instances.

1. Welcome to the Nope Monad, where nothing happens and nobody cares.

data Nope a = NopeDotJpg
-- We're serious. Write it anyway.

-}

data Nope a = NopeDotJpg
            deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _                    = NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return _         = NopeDotJpg
  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

propNopeFunctorIdentity :: Property
propNopeFunctorIdentity = forAll gen prop
  where gen             = return NopeDotJpg
        prop x          = fmap id x == id x

propNopeFunctorComposition :: Property
propNopeFunctorComposition = forAll gen prop
  where gen                = return NopeDotJpg
        prop x             = fmap (id . id) x == (fmap id $ fmap id x)

propNopeApplicativeIdentity :: Property
propNopeApplicativeIdentity = forAll gen prop
  where gen                 = return NopeDotJpg
        prop x              = (pure id <*> x) == x

propNopeApplicativeComposition :: Property
propNopeApplicativeComposition = forAll gen prop
  where gen                    = return NopeDotJpg
        prop x                 = (pure (.) <*> pure id <*> pure id <*> x) ==
          (pure id <*> (pure id <*> x))

propNopeApplicativeHomomorphism                        :: Property
propNopeApplicativeHomomorphism = forAll gen prop
  where gen                     = arbitrary            :: Gen Int
        prop x                  = (pure id <*> (pure x :: Nope Int)) ==
          (pure (id x))

propNopeApplicativeInterchange                        :: Property
propNopeApplicativeInterchange = forAll gen prop
  where gen                    = arbitrary            :: Gen Int
        prop x                 = (pure id <*> (pure x :: Nope Int)) ==
          (pure ($ x) <*> (pure id))

propNopeMonadLeftIdentity              :: Property
propNopeMonadLeftIdentity = forAll gen prop
  where gen               = arbitrary  :: Gen Int
        prop x            = ((return x :: Nope Int) >>= return) == return x

propNopeMonadRightIdentity :: Property
propNopeMonadRightIdentity = forAll gen prop
  where gen                = return NopeDotJpg
        prop x             = (x >>= return) == x

propNopeMonadAssociativity :: Property
propNopeMonadAssociativity = forAll gen prop
  where gen                = return NopeDotJpg
        prop x             = ((x >>= return) >>= return) ==
          (x >>= (\a' -> return a' >>= return))

propNopeList :: [Property]
propNopeList = [ propNopeFunctorIdentity
               , propNopeFunctorComposition
               , propNopeApplicativeIdentity
               , propNopeApplicativeComposition
               , propNopeApplicativeHomomorphism
               , propNopeApplicativeInterchange
               , propNopeMonadLeftIdentity
               , propNopeMonadRightIdentity
               , propNopeMonadAssociativity
               ]

testNope :: IO ()
testNope = mapM_ quickCheck propNopeList

{-

2.

data PhhhbbtttEither b a = Left a
                         | Right b

-}

data PhhhbbtttEither b a = Left a
                         | Right b
                         deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap _ (Right x) = Right x
  fmap f (Left x)  = Left $ f x

instance Applicative (PhhhbbtttEither b) where
  pure                  = Left
  (Right x) <*> _       = Right x
  _ <*> (Right x)       = Right x
  (Left f) <*> (Left x) = Left $ f x

instance Monad (PhhhbbtttEither b) where
  return          = pure
  (Right x) >>= _ = Right x
  (Left x) >>= f  = f x

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do x <- arbitrary
                 y <- arbitrary
                 elements [Right x, Left y]

propPhFunctorIdentity             :: Property
propPhFunctorIdentity = forAll g p
  where g             = arbitrary :: Gen (PhhhbbtttEither String Int)
        p x           = fmap id x == id x

propPhFunctorComposition             :: Property
propPhFunctorComposition = forAll g p
  where g                = arbitrary :: Gen (PhhhbbtttEither String Int)
        p x              = fmap (id . id) x == (fmap id $ fmap id x)

propPhApplicativeIdentity             :: Property
propPhApplicativeIdentity = forAll g p
  where g                 = arbitrary :: Gen (PhhhbbtttEither String Int)
        p x               = (pure id <*> x) == x

propPhApplicativeComposition             :: Property
propPhApplicativeComposition = forAll g p
  where g                    = arbitrary :: Gen (PhhhbbtttEither String Int)
        p x                  = (pure (.) <*> pure id <*> pure id <*> x) ==
          (pure id <*> (pure id <*> x))

propPhApplicativeHomomorphism             :: Property
propPhApplicativeHomomorphism = forAll g p
  where g                     = arbitrary :: Gen Int
        p x                   = (pure id <*>
          (pure x                         :: PhhhbbtttEither String Int)) ==
          (pure (id x))

propPhApplicativeInterchange             :: Property
propPhApplicativeInterchange = forAll g p
  where g                    = arbitrary :: Gen Int
        p x                  = (pure id <*>
          (pure x                        :: PhhhbbtttEither String Int)) ==
          (pure ($ x) <*> (pure id))

propPhMonadLeftIdentity              :: Property
propPhMonadLeftIdentity = forAll g p
  where g               = arbitrary  :: Gen Int
        p x             = ((return x :: PhhhbbtttEither String Int) >>= return) ==
          return x

propPhMonadRightIdentity             :: Property
propPhMonadRightIdentity = forAll g p
  where g                = arbitrary :: Gen (PhhhbbtttEither String Int)
        p x              = (x >>= return) == x

propPhMonadAssociativity             :: Property
propPhMonadAssociativity = forAll g p
  where g                = arbitrary :: Gen (PhhhbbtttEither String Int)
        p x              = ((x >>= return) >>= return) ==
          (x >>= (\a' -> return a' >>= return))

testPh :: IO ()
testPh = mapM_ quickCheck [ propPhFunctorIdentity
                          , propPhFunctorComposition
                          , propPhApplicativeIdentity
                          , propPhApplicativeComposition
                          , propPhApplicativeInterchange
                          , propPhApplicativeHomomorphism
                          , propPhMonadLeftIdentity
                          , propPhMonadRightIdentity
                          , propPhMonadAssociativity
                          ]

{-

3. Write a Monad instance for Identity.

newtype Identity a = Identity a
                   deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap = undefined

instance Applicative Identity where
  pure  = undefined
  (<*>) = undefined

instance Monad Identity where
  return = pure
  (>>=)  = undefined

-}

newtype Identity a = Identity a
                   deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure               = Identity
  (Identity f) <*> x = fmap f x

instance Monad Identity where
  return             = pure
  (Identity x) >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do x <- arbitrary
                 return $ Identity x

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

propIdFunctor :: TestBatch
propIdFunctor = functor $ Identity ("mike", "aya", "kc")

propIdApplicative :: TestBatch
propIdApplicative = applicative $ Identity ("mike", "aya", "kc")

propIdMonad :: TestBatch
propIdMonad = monad $ Identity ("mike", "aya", "kc")

testId :: IO ()
testId = mapM_ quickBatch [ propIdFunctor
                          , propIdApplicative
                          , propIdMonad
                          ]

{-

4. This one should be easier than the Applicative instance was. Remember to use the
Functor that Monad requires, then see where the chips fall.

data List a = Nil
            | Cons a (List a)

-}

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

instance Foldable List where
  foldMap               = undefined
  foldr                 = undefined
  foldl _ s Nil         = s
  foldl f s (Cons x xs) = foldl f (f s x) xs

instance Semigroup (List a) where
  (<>) = mappend

instance Monoid (List a) where
  mempty                   = Nil
  Nil `mappend` Nil        = Nil
  Nil `mappend` xs         = xs
  xs `mappend` Nil         = xs
  (Cons x xs) `mappend` ys = Cons x (xs `mappend` ys)

mconcat' :: List (List a) -> List a
mconcat' = foldl mappend Nil

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x             = Cons x Nil
  Nil <*> _          = Nil
  _ <*> Nil          = Nil
  (Cons f fs) <*> xs = fmap f xs `mappend` (fs <*> xs)

instance Monad List where
  return    = pure
  Nil >>= _ = Nil
  xs >>= f  = mconcat' $  fmap f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do e  <- arbitrary
                 xs <- arbitrary
                 elements [Cons e xs, Nil]

instance Eq a => EqProp (List a) where
  (=-=) = eq

propListFunctor :: TestBatch
propListFunctor = functor $ Cons ("mike", "aya", "kc") Nil

propListApplicative :: TestBatch
propListApplicative = applicative $ Cons ("mike", "kc", "aya") Nil

propListMonad :: TestBatch
propListMonad = monad $ Cons ("mike", "aya", "kc") Nil

testList :: IO ()
testList = mapM_ quickBatch [ propListFunctor
                            , propListApplicative
                            , propListMonad
                            ]

{-

Write the following functions using the methods provided by
Monad and Functor. Using stuff like identity and composition is fine,
but it has to typecheck with types provided.

1.

j :: Monad m => m (m a) -> m a

Expecting the following behavior:

Prelude> j [[1, 2], [], [3]]
[1,2,3]

Prelude> j (Just (Just 1))
Just 1

Prelude> j (Just Nothing)
Nothing

Prelude> j Nothing
Nothing

-}

j :: Monad m => m (m a) -> m a
j mma = mma >>= (\ma    -> ma >>= return)

{-

2.

l1 :: Monad m => (a -> b) -> m a -> m b

-}

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = ma >>= (return . f)

l1' :: Monad m => m a -> (a -> b) -> m b
l1' ma f = l1 f ma

{-

3.

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c

-}

l2 :: Monad m => (a      -> b -> c) -> m a -> m b -> m c
l2 f ma mb = ma >>= (\a' -> mb >>= return . f a')

{-

4.

a :: Monad m => m a -> m (a -> b) -> m b

-}

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf >>= l1' ma

{-

5. You’ll need recursion for this one.

meh :: Monad m => [a] -> (a -> m b) -> m [b]

-}

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs f = flipType $ fmap f xs

{-

6. Hint: reuse “meh”

flipType :: (Monad m) => [m a] -> m [a]

-}

flipType :: Monad m => [m a] -> m [a]
flipType       = foldl f (return [])
  where f ms m = ms >>= (\xs -> m >>= return . (: xs))
