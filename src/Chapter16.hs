{-# LANGUAGE NoImplicitPrelude #-}

module Chapter16 where

import Control.Monad            (return)
import Data.Bool                (Bool (True))
import Data.Eq                  (Eq, (==))
import Data.Function            (id, ($), (.))
import Data.Functor             (Functor, fmap)
import Data.Int                 (Int)
import Data.List                ((++))
import Data.Maybe               (Maybe (Just))
import Data.String              (String)
import GHC.Err                  (undefined)
import GHC.Integer              (Integer)
import GHC.Num                  ((*), (+), (-))
import System.IO                (IO, readIO)
import Test.Hspec               (describe, hspec, it, shouldBe)
import Test.QuickCheck          (Arbitrary, arbitrary, elements, quickCheck)
import Test.QuickCheck.Function (Fun (Fun))
import Text.Read                (read)
import Text.Show                (Show, show)

{-

Exercises: Be Kind

Given a type signature, determine the kinds of each type variable:

1. What’s the kind of a?
a -> a

=> *

2. What are the kinds of b and T? (The T is capitalized on purpose!)
a -> b a -> T (b a)

=> * -> *

3. What’s the kind of c?
c a b -> c b a

=> * -> * -> *



Exercises: Heavy Lifting

Add fmap, parentheses, and function composition to the expression
as needed for the expression to typecheck and produce the expected
result. It may not always need to go in the same place, so don’t get
complacent.

1.

a = (+1) $ read "[1]" :: [Int]

Expected result

Prelude> a
[2]

-}

a                           :: [Int]
a = fmap (+ 1) $ read "[1]" :: [Int]

{-

2.

b = (++ "lol") (Just ["Hi,", "Hello"])

Prelude> b
Just ["Hi,lol","Hellolol"]

-}

b :: Maybe [String]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

{-

3.

c = (*2) (\x -> x - 2)

Prelude> c 1
-2

-}

c :: Integer        -> Integer
c = fmap  (* 2) (\x -> x - 2)

{-

4.

d = ((return '1' ++) . show) (\x -> [x, 1..3])

Prelude> d 0
"1[0,1,2,3]"

-}

d :: Integer                          -> String
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

{-

5.

readz :: IO Integer
readz = readIO "1"

e :: IO Integer
e               = (*3) changed
  where ioi     = read
        changed = read ("123"++) show ioi

Prelude> e
3693

-}

e                                                       :: IO Integer
e = fmap ((* 3) . read . ("123" ++) . show) (readIO "1" :: IO Integer)

{-

Exercises: Instances of Func

Implement Functor instances for the following datatypes. Use the
QuickCheck properties we just showed you to validate them.

-}

functorIdentity :: (Eq (f a), Functor f) => f a -> Bool
functorIdentity x = fmap id x == x

type IntFI = [Int] -> Bool

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = fmap (g . f) x == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC    = [Int] -> IntToInt -> IntToInt -> Bool

functorTests                                  :: IO ()
functorTests = do quickCheck (functorIdentity :: IntFI)
                  quickCheck (functorCompose' :: IntFC)
                  quickCheck (functorIdentity :: IdentityFunctorIdentity)
                  quickCheck (functorCompose' :: IdentityFunctorCompose)
                  quickCheck (functorIdentity :: PairFunctorIdentity)
                  quickCheck (functorCompose' :: PairFunctorCompose)
                  quickCheck (functorIdentity :: ThreeFunctorIdentity)
                  quickCheck (functorCompose' :: ThreeFunctorCompose)
                  quickCheck (functorIdentity :: Three'FunctorIdentity)
                  quickCheck (functorCompose' :: Three'FunctorCompose)
                  quickCheck (functorIdentity :: FourFunctorIdentity)
                  quickCheck (functorCompose' :: FourFunctorCompose)
                  quickCheck (functorIdentity :: Four'FunctorIdentity)
                  quickCheck (functorCompose' :: Four'FunctorCompose)

{-

1.

newtype Identity a = Identity a

-}

newtype Identity a = Identity a
                   deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do x <- arbitrary
                 return $ Identity x

type IdentityFunctorIdentity = Identity Int -> Bool
type IdentityFunctorCompose  = Identity Int -> IntToInt -> IntToInt -> Bool

{-

2.

data Pair a = Pair a a

-}

data Pair a = Pair a a
            deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do x <- arbitrary
                 return $ Pair x x

type PairFunctorIdentity = Pair Int -> Bool
type PairFunctorCompose  = Pair Int -> IntToInt -> IntToInt -> Bool

{-

4.

data Three a b c = Three a b c

-}

data Three a b c = Three a b c
                 deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y $ f z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do x <- arbitrary
                 y <- arbitrary
                 z <- arbitrary
                 return $ Three x y z

type ThreeFunctorIdentity = Three Int Int Int -> Bool
type ThreeFunctorCompose  = Three Int Int Int -> IntToInt -> IntToInt -> Bool

{-

5.

data Three' a b = Three' a b b

-}

data Three' a b = Three' a b b
                deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y y') = Three' x (f y) (f y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do x  <- arbitrary
                 y  <- arbitrary
                 y' <- arbitrary
                 return $ Three' x y y'

type Three'FunctorIdentity = Three' Int Int -> Bool
type Three'FunctorCompose  = Three' Int Int -> IntToInt -> IntToInt -> Bool

{-

6.

data Four a b c d = Four a b c d

-}

data Four a b c d = Four a b c d
                  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y $ f z

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
  arbitrary = do w <- arbitrary
                 x <- arbitrary
                 y <- arbitrary
                 z <- arbitrary
                 return $ Four w x y z

type FourFunctorIdentity = Four Int Int Int Int -> Bool
type FourFunctorCompose  = Four Int Int Int Int -> IntToInt -> IntToInt -> Bool

{-

7.

data Four' a b = Four' a a a b

-}

data Four' a b = Four' a a a b
               deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x x' x'' y) = Four' x x' x'' $ f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do w <- arbitrary
                 x <- arbitrary
                 y <- arbitrary
                 z <- arbitrary
                 return $ Four' w x y z

type Four'FunctorIdentity = Four' Int Int -> Bool
type Four'FunctorCompose  = Four' Int Int -> IntToInt -> IntToInt -> Bool

{-

8. Can you implement one for this type? Why? Why not?

data Trivial = Trivial

Cannot implement a Functor from Trivial because the kind of Trivial is *. A Functorneeds something of kind * -> *



Exercise: Possibly

Write a Functor instance for a datatype identical to Maybe. We’ll use
our own datatype because Maybe already has a Functor instance and
we cannot make a duplicate one.

data Possibly a = LolNope
                | Yeppers a
                deriving (Eq, Show)

instance Functor Possibly where
fmap = undefined

If it helps, you’re basically writing the following function:

applyIfJust :: (a -> b) -> Maybe a -> Maybe b

-}

data Possibly a = LolNope
                | Yeppers a
                deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers x) = Yeppers $ f x

{-

Short Exercise

1. Write a Functor instance for a datatype identical to Either. We’ll
use our own datatype because Either also already has a Functor
instance.

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where
fmap = undefined

-}

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second x) = Second $ f x

applyIfSecond :: (a -> b) -> (Sum e) a -> (Sum e) b
applyIfSecond = fmap

{-

2. Why is a Functor instance that applies the function only to First,
Either’s Left, impossible? We covered this earlier.

=> because type `a` was applied of the type Sum to the Functor to have a
   kind * -> *



Chapter exercises

Determine if a valid Functor can be written for the datatype provided.

1.

data Bool = False | True

=> No because Bool is of kind *


2.

data BoolAndSomethingElse a = False' a | True' a

-}

data BoolAndSomethingElse a = False' a | True' a
                            deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap f (False' x) = False' $ f x
  fmap f (True' x)  = True' $ f x

instance Arbitrary a => Arbitrary (BoolAndSomethingElse a) where
  arbitrary = do x <- arbitrary
                 elements [False' x, True' x]

type BoolAndSomethingElseFunctorIdentity = BoolAndSomethingElse Int -> Bool

type BoolAndSomethingElseFunctorCompose =
  BoolAndSomethingElse Int -> IntToInt -> IntToInt -> Bool

testBoolAndSomethingElse         ::  IO ()
testBoolAndSomethingElse =
  do quickCheck (functorIdentity :: BoolAndSomethingElseFunctorIdentity)
     quickCheck (functorCompose' :: BoolAndSomethingElseFunctorCompose)

{-

3.

data BoolAndMaybeSomethingElse a = Falsish | Truish a

-}

data BoolAndMaybeSomethingElse a = Falsish | Truish a
                                 deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish    = Falsish
  fmap f (Truish x) = Truish $ f x

instance Arbitrary a => Arbitrary (BoolAndMaybeSomethingElse a) where
  arbitrary = do x <- arbitrary
                 elements [Falsish, Truish x]

type BoolAndMaybeSomethingElseFunctorIdentity =
  BoolAndMaybeSomethingElse Int -> Bool

type BoolAndMaybeSomethingElseFunctorCompose =
  BoolAndMaybeSomethingElse Int -> IntToInt -> IntToInt -> Bool

testBoolAndMaybeSomethingElse    :: IO ()
testBoolAndMaybeSomethingElse =
  do quickCheck (functorIdentity :: BoolAndMaybeSomethingElseFunctorIdentity)
     quickCheck (functorCompose' :: BoolAndMaybeSomethingElseFunctorCompose)

{-

4. Use the kinds to guide you on this one, don’t get too hung up
on the details.

newtype Mu f = InF { outF :: f (Mu f) }

-}


newtype Mu a = InF { outF :: a (Mu a) }

--instance Functor Mu where
--  fmap f (InF x xs) = undefined

{-

5. Again, just follow the kinds and ignore the unfamiliar parts

import GHC.Arr

data D = D (Array Word Word) Int Int

-}

{-

Rearrange the arguments to the type constructor of the datatype
so the Functor instance works.

1.

data Sum a b = First a
             | Second b

instance Functor (Sum e) where

fmap f (First a)  = First (f a)
fmap f (Second b) = Second b

-}

data Sum' b a = First' a
              | Second' b

instance Functor (Sum' e) where
  fmap f (First' x)  = First' (f x)
  fmap _ (Second' x) = Second' x

{-

2.

data Company a b c = DeepBlue a c
                   | Something b

instance Functor (Company e e') where
  fmap f (Something b)  = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-}

data Company a c b = DeepBlue a c
                   | Something b

instance Functor (Company e e') where
  fmap f (Something x)  = Something $ f x
  fmap _ (DeepBlue x y) = DeepBlue x y

{-

3.

data More a b = L a b a
              | R b a b
              deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

Keeping in mind that it should result in a Functor that does the
following:

Prelude> fmap (+1) (L 1 2 3)
L 2 2 4

Prelude> fmap (+1) (R 1 2 3)
R 1 3 3

-}

data More b a = L a b a
              | R b a b
              deriving (Eq, Show)

instance Functor (More a) where
  fmap f (L x y x') = L (f x) y (f x')
  fmap f (R x y x') = R x (f y) x'

testMore :: IO ()
testMore = hspec $
  describe "test the More datatype's Functor instance" $ do
    it "fmap (+1) (L 1 2 3)" $
      fmap (+ 1) (L 1 2 3) `shouldBe` L 2 2 4
    it "fmap (+1) (R 1 2 3)" $
      fmap (+ 1) (R 1 2 3) `shouldBe` R 1 3 3

{-

Write Functor instances for the following datatypes.

1.

data Quant a b = Finance
               | Desk a
               | Bloor b

-}

data Quant a b = Finance
               | Desk a
               | Bloor b
               deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk x)  = Desk x
  fmap f (Bloor x) = Bloor $ f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = do x <- arbitrary
                 y <- arbitrary
                 elements [Finance, Desk x, Bloor y]

type QuantFunctorIdentity = Quant Int Int -> Bool

type QuantFunctorCompose = Quant Int Int -> IntToInt -> IntToInt -> Bool

testQuantFunctorProps                                  :: IO ()
testQuantFunctorProps = do quickCheck (functorIdentity :: QuantFunctorIdentity)
                           quickCheck (functorCompose' :: QuantFunctorCompose)

{-

2. No, it’s not interesting by itself.

data K a b = K a

-- should remind you of an
-- instance you've written before

instance Functor (Flip K a) where
fmap = undefined

-}

data K a b = K a
           deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K x) = K x

instance (Arbitrary a, Arbitrary b) => Arbitrary (K a b) where
  arbitrary = do x <- arbitrary
                 return $ K x

type KFunctorIdentity = K Int Int -> Bool

type KFunctorCompose = K Int Int -> IntToInt -> IntToInt -> Bool

testKFunctorProps                                  :: IO ()
testKFunctorProps = do quickCheck (functorIdentity :: KFunctorIdentity)
                       quickCheck (functorCompose' :: KFunctorCompose)

{-

4.

data EvilGoateeConst a b = GoatyConst b

-- You thought you'd escaped the goats
-- by now didn't you? Nope.

No, it doesn’t do anything interesting. No magic here or in the
previous exercise. If it works, you succeeded.

-}

data EvilGoateeConst a b = GoatyConst b
                         deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst $ f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = do x <- arbitrary
                 return $ GoatyConst x

type EVCFunctorIdentity = EvilGoateeConst Int Int -> Bool

type EVCFunctorCompose = EvilGoateeConst Int Int -> IntToInt -> IntToInt -> Bool

testEVCFunctorProps                                  :: IO ()
testEVCFunctorProps = do quickCheck (functorIdentity :: EVCFunctorIdentity)
                         quickCheck (functorCompose' :: EVCFunctorCompose)

{-

5. Do you need something extra to make the instance work?

data LiftItOut f a = LiftItOut (f a)

-}

data LiftItOut f a = LiftItOut (f a)

instance Functor (LiftItOut a) where
  fmap _ (LiftItOut _) = LiftItOut undefined

{-

6.

data Parappa f g a = DaWrappa (f a) (g a)

-}

data Parappa f g a = DaWrappa (f a) (g a)

instance Functor (Parappa a b) where
  fmap _ (DaWrappa _ _) = undefined

{-

7. Don’t ask for more typeclass instances than you need. You can
let GHC tell you what to do.

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

-}

{-

8.

data Notorious g o a t = Notorious (g o) (g a) (g t)

-}

{-

9. You’ll need to use recursion.

data List a = Nil
            | Cons a (List a)

-}

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do x  <- arbitrary
                 xs <- arbitrary
                 elements [Nil, Cons x xs]

type ListFunctorIdentity = List Int -> Bool

type ListFunctorCompose = List Int -> IntToInt -> IntToInt -> Bool

testListFunctorProps                                  :: IO ()
testListFunctorProps = do quickCheck (functorIdentity :: ListFunctorIdentity)
                          quickCheck (functorCompose' :: ListFunctorCompose)

{-

10. A tree of goats forms a Goat-Lord, fearsome poly-creature.

data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

-}

data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
                deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat               = NoGoat
  fmap f (OneGoat x)          = OneGoat $ f x
  fmap f (MoreGoats xs ys zs) = MoreGoats (fmap f xs) (fmap f ys) (fmap f zs)

instance Arbitrary a => Arbitrary (GoatLord a) where
  arbitrary = do x  <- arbitrary
                 xs <- arbitrary
                 ys <- arbitrary
                 zs <- arbitrary
                 elements [NoGoat, OneGoat x, MoreGoats xs ys zs]

type GoatLordFunctorIdentity = GoatLord Int -> Bool

type GoatLordFunctorCompose = GoatLord Int -> IntToInt -> IntToInt -> Bool

testGoatLordFunctorProps                                  :: IO ()
testGoatLordFunctorProps = do quickCheck (functorIdentity :: GoatLordFunctorIdentity)
                              quickCheck (functorCompose' :: GoatLordFunctorCompose)

{-

11. You’ll use an extra functor for this one, although your solution
might do it monomorphically without using fmap.

data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)

-}

data TalkToMe a = Halt
                | Print String a
                | Read (Fun String a)
                deriving (Eq, Show)

instance Eq (Fun a b) where
  _ == _ = True

instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print s x) = Print s (f x)
  -- fmap f (Read g) = Read $ f . g
  fmap _ _           = undefined

instance Arbitrary a => Arbitrary (TalkToMe a) where
  arbitrary = do x <- arbitrary
                 s <- arbitrary
                 f <- arbitrary
                 elements [Halt, Print s x, Read f]

type TalkToMeFunctorIdentity = TalkToMe Int -> Bool

type TalkToMeFunctorCompose = TalkToMe Int -> IntToInt -> IntToInt -> Bool

testTalkToMeFunctorProps                                  :: IO ()
testTalkToMeFunctorProps = do quickCheck (functorIdentity :: TalkToMeFunctorIdentity)
                              quickCheck (functorCompose' :: TalkToMeFunctorCompose)
