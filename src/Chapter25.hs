{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Chapter25 where

import Control.Applicative (Applicative, liftA2, pure, (<*>))
import Data.Eq             (Eq)
import Data.Foldable       (Foldable, foldMap)
import Data.Function       (id, ($), (.))
import Data.Functor        (Functor, fmap)
import Data.Traversable    (Traversable, traverse)
import Text.Show           (Show)

newtype Compose f g a = Compose
                      { getCompose :: f (g a)
                      }
                      deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose ((fmap . fmap) f fga)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where

  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose (liftA2 (<*>) f a)

{-

Exercises: Compose Instances

1. Write the Compose Foldable instance.
The foldMap = undefined bit is a hint to make it easier and look
more like what you’ve seen already.

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap = undefined

-}

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose a) = (foldMap .foldMap) f a

{-

2. Write the Compose Traversable instance.

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse = undefined

-}

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose a) = fmap Compose ((traverse . traverse) f a)

{-


And now for something completely different
This has nothing to do with anything else in this chapter, but it makes
for a fun exercise.

-}

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

{-

It’s a functor that can map over two type arguments instead of
one. Write Bifunctor instances for the following types:

1. The less you think, the easier it’ll be.
data Deux a b = Deux a b

-}

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap :: (a -> b) -> (c -> d) -> Deux a c -> Deux b d
  bimap f g (Deux x y) = Deux (f x) (g y)

  first :: (a -> b) -> Deux a c -> Deux b c
  first f = bimap f id

  second :: (b -> c) -> Deux a b -> Deux a c
  second = bimap id

{-

2. data Const a b = Const a

-}

newtype Const a b = Const a

instance Bifunctor Const where
  bimap :: (a -> b) -> (c -> d) -> Const a c -> Const b d
  bimap f _ (Const x) = Const $ f x

  first :: (a -> b) -> Const a c -> Const b c
  first f = bimap f id

  second :: (b -> c) -> Const a b -> Const a c
  second = bimap id

{-

3. data Drei a b c = Drei a b c

-}

data Drei a b c = Drei a b c

instance Bifunctor (Drei n) where
  bimap :: (a -> b) -> (c -> d) -> Drei n a c -> Drei n b d
  bimap f g (Drei x y z) = Drei x (f y) (g z)

  first :: (a -> b) -> Drei n a c -> Drei n b c
  first f = bimap f id

  second :: (b -> c) -> Drei n a b -> Drei n a c
  second = bimap id

{-

4. data SuperDrei a b c = SuperDrei a b

-}

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei n) where
  bimap :: (a -> b) -> (c -> d) -> SuperDrei n a c -> SuperDrei n b d
  bimap f _ (SuperDrei x y) = SuperDrei x (f y)

  first :: (a -> b) -> SuperDrei n a c -> SuperDrei n b c
  first f = bimap f id

  second :: (b -> c) -> SuperDrei n a b -> SuperDrei n a c
  second = bimap id

{-

5. data SemiDrei a b c = SemiDrei a

-}

newtype SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei n) where
  bimap :: (a -> b) -> (c -> d) -> SemiDrei n a c -> SemiDrei n b d
  bimap _ _ (SemiDrei x) = SemiDrei x

  first :: (a -> b) -> SemiDrei n a c -> SemiDrei n b c
  first f = bimap f id

  second :: (b -> c) -> SemiDrei n a b -> SemiDrei n a c
  second = bimap id

{-

6. data Quadriceps a b c d = Quadzzz a b c d

-}

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps m n) where
  bimap :: (a -> b) -> (c -> d) -> Quadriceps m n a c -> Quadriceps m n b d
  bimap f g (Quadzzz w x y z) = Quadzzz w x (f y) (g z)

  first :: (a -> b) -> Quadriceps m n a c -> Quadriceps m n b c
  first f = bimap f id

  second :: (b -> c) -> Quadriceps m n a b -> Quadriceps m n a c
  second = bimap id

{-

7. data Either a b = Left a
                   | Right b

-}

data Either a b = Left a
                | Right b

instance Bifunctor Either where
  bimap :: (a -> b) -> (c -> d) -> Either a c -> Either b d
  bimap f _ (Left x)  = Left $ f x
  bimap _ g (Right x) = Right $ g x

  first :: (a -> b) -> Either a c -> Either b c
  first f = bimap f id

  second :: (b -> c) -> Either a b -> Either a c
  second = bimap id
