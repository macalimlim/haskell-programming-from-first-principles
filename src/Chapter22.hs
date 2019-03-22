{-# LANGUAGE NoImplicitPrelude #-}
--{-# LANGUAGE InstanceSigs #-}

module Chapter22 where

import Control.Applicative (Applicative, liftA2, pure, (<*>))
import Control.Monad       (Monad, liftM2, return, (>>=))
import Data.Char           (toUpper)
import Data.Eq             (Eq)
import Data.Function       (const, id, ($), (.))
import Data.Functor        (Functor, fmap, (<$>))
import Data.List           (map, reverse)
import Data.String         (String)
import Text.Show           (Show)

{-

Short Exercise: Warming Up

We’ll be doing something here very similar to what you saw above,
to give you practice and try to develop a feel or intuition for what is
to come. These are similar enough to what you just saw that you can
almost copy and paste, so try not to overthink them too much.

First, start a file off like this:

import Data.Char

-}

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

{-

Two simple functions with the same type, taking the same type
of input. We could compose them, using (.) or fmap:

composed :: [Char] -> [Char]
composed = undefined

fmapped :: [Char] -> [Char]
fmapped = undefined

-}

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = fmap cap rev

{-

Now we want to return the results of cap and rev both, as a tuple,
like this:

Prelude> tupled "Julie"
("JULIE","eiluJ")

Prelude> tupled' "Julie"
("eiluJ","JULIE")

We will want to use an applicative here. The type will look like
this:

tupled :: [Char] -> ([Char], [Char])

There is no special reason such a function needs to be monadic,
but let’s do that, too, to get some practice. Do it one time using do
syntax; then try writing a new version using (>>=). The types will be
the same as the type for tupled.

-}

tupled :: String -> (String, String)
tupled = (,) <$> composed <*> fmapped

tupled' :: String -> (String, String)
tupled' = liftA2 (,) composed fmapped

tupled'' :: String -> (String, String)
tupled'' = do x <- composed
              y <- fmapped
              return (x, y)

tupled''' :: String          -> (String, String)
tupled''' = composed >>= (\x -> fmapped >>= (\y -> return (x, y)))

tupled'''' :: String -> (String, String)
tupled'''' = liftM2 (,) composed fmapped

{-

Exercise: Ask

Implement the following function. If you get stuck, remember it’s
less complicated than it looks. Write down what you know. What do
you know about the type a? What does the type simplify to? How
many inhabitants does that type have? You’ve seen the type before.

ask :: Reader a a
ask = Reader ???

-}

newtype Reader r a = Reader
                   { runReader :: r -> a
                   }

ask :: Reader a a
ask = Reader id

{-

Exercise: Reading Comprehension

1. Write liftA2 yourself. Think about it in terms of abstracting out
the difference between getDogR and getDogR' if that helps.

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 = undefined

-}

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

{-

2. Write the following function. Again, it is simpler than it looks.

asks :: (r -> a) -> Reader r a
asks f = Reader ???

-}

asks :: (r -> a) -> Reader r a
asks = Reader

{-

3. Implement the Applicative for Reader.

To write the Applicative instance for Reader, we’ll use a pragma
called InstanceSigs. It’s an extension we need in order to assert
a type for the typeclass methods. You ordinarily cannot assert
type signatures in instances. The compiler already knows the
type of the functions, so it’s not usually necessary to assert the
types in instances anyway. We did this for the sake of clarity, to
make the Reader type explicit in our signatures.

-- you'll need this pragma
{-# LANGUAGE InstanceSigs #-}

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ ???

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b

  (Reader rab) <*> (Reader ra) = Reader $ \r -> ???

a) When writing the pure function for Reader, remember that
what you’re trying to construct is a function that takes a
value of type r, which you know nothing about, and return a
value of type a. Given that you’re not really doing anything
with r, there’s really only one thing you can do.

b) We got the definition of the apply function started for you,
we’ll describe what you need to do and you write the code.
If you unpack the type of Reader’s apply above, you get the
following.

<*> :: (r -> a -> b) -> (r -> a) -> (r -> b)

-- contrast this with the type of fmap
fmap :: (a -> b) -> (r -> a) -> (r -> b)

So what’s the difference? The difference is that apply, unlike
fmap, also takes an argument of type r.
Make it so.

-}

instance Functor (Reader r) where
  fmap f (Reader g) = Reader $ f . g

instance Applicative (Reader r) where
  pure a                    = Reader $ const a
  (Reader f) <*> (Reader g) = Reader $ \r -> f r (g r)

{-

Exercise: Reader Monad

1. Implement the Reader Monad.

-- Don't forget instancesigs.

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b

  (Reader ra) >>= aRb = Reader $ \r -> ???

Hint: constrast the type with the Applicative instance and per-
form the most obvious change you can imagine to make it work.

-}

instance Monad (Reader r) where
  return           = pure
  (Reader f) >>= g = Reader $ \r -> runReader (g $ f r) r

{-

2. Rewrite the monadic getDogRM to use your Reader datatype.

-}

newtype HumanName = HumanName String
                  deriving (Eq, Show)

newtype DogName = DogName String
                deriving (Eq, Show)

newtype Address = Address String
                deriving (Eq, Show)

data Person = Person
            { humanName :: HumanName
            , dogName   :: DogName
            , address   :: Address
            } deriving (Eq, Show)

data Dog = Dog
         { dogsName    :: DogName
         , dogsAddress :: Address
         } deriving (Eq, Show)

getDogRM :: Person -> Dog
getDogRM = do name <- dogName
              addy <- address
              return $ Dog name addy

getDogRM' :: Reader Person Dog
getDogRM' = Reader $ liftM2 Dog dogName address

{-

See ReaderPractice.hs

-}
