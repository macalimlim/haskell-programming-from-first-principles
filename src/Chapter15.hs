{-# LANGUAGE NoImplicitPrelude #-}

module Chapter15 where

import Control.Monad   (mapM_, return)
import Data.Bool       (Bool (False, True), (&&))
import Data.Char       (Char)
import Data.Eq         (Eq, (==))
import Data.Function   (const, ($), (.))
import Data.Int        (Int)
import Data.Monoid     (Monoid, Product (Product), Sum (Sum), mappend, mconcat,
                        mempty)
import Data.Semigroup  (Semigroup, (<>))
import Data.String     (String)
import GHC.Err         (undefined)
import System.IO       (IO)
import Test.Hspec      (describe, hspec, it, shouldBe)
import Test.QuickCheck (Arbitrary, CoArbitrary, Gen, Property, Testable,
                        arbitrary, coarbitrary, elements, forAll, maxSuccess,
                        quickCheck, quickCheckWith, stdArgs, verbose)
import Text.Show       (Show)

{-

Exercise: Optional Monoid

Write the Monoid instance for our Maybe type renamed to Optional.

data Optional a = Nada
                | Only a
                deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
         mempty  = undefined
         mappend = undefined

Expected output:

Prelude> Only (Sum 1) `mappend` Only (Sum 1)
Only (Sum {getSum = 2})

Prelude> Only (Product 4) `mappend` Only (Product 2)
Only (Product {getProduct = 8})

Prelude> Only (Sum 1) `mappend` Nada
Only (Sum {getSum = 1})

Prelude> Only [1] `mappend` Nada
Only [1]

Prelude> Nada `mappend` Only (Sum 1)
Only (Sum {getSum = 1})

-}

data Optional a = Nada
                | Only a
                deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
         mempty                      = Nada
         x `mappend` Nada            = x
         Nada `mappend` y            = y
         (Only x) `mappend` (Only y) = Only (x `mappend` y)

deepCheck :: Testable a => a -> IO ()
deepCheck = quickCheckWith stdArgs {maxSuccess = 1000} . verbose

runSpecTests :: IO ()
runSpecTests = hspec $
  describe "Unit test for our Optional" $ do
    it "Test Sum" $
      Only (Sum 1) `mappend` Only (Sum 1) `shouldBe`
        Only (Sum 2)
    it "Test Product" $
      Only (Product 4) `mappend` Only (Product 2) `shouldBe`
        Only (Product 8)
    it "Test Sum left identity" $
      Only (Sum 1) `mappend` Nada `shouldBe`
        Only (Sum 1)
    it "Test List left identity" $
      Only [1] `mappend` Nada `shouldBe`
        Only [1]
    it "Test Sum right identity" $
      Nada `mappend` Only (Sum 1) `shouldBe`
        Only (Sum 1)
    it "Test associativity" $
      (Only (Sum 1) `mappend` Only (Sum 2)) `mappend` Only (Sum 3) `shouldBe`
        Only (Sum 1) `mappend` (Only (Sum 2) `mappend` Only (Sum 3))

genInt :: Gen Int
genInt = arbitrary

genListInt :: Gen [Int]
genListInt = arbitrary

genChar :: Gen Char
genChar = arbitrary

genString :: Gen String
genString = arbitrary

genTriple :: Gen (Int, [Int], String)
genTriple = do n  <- genInt
               ns <- genListInt
               s  <- genString
               return (n, ns, s)

propLeftIdentity :: Property
propLeftIdentity        = forAll gen prop
  where gen             = genTriple
        sumProp n       = Nada `mappend` Only (Sum n) == Only (Sum n)
        prodProp n      = Nada `mappend` Only (Product n) == Only (Product n)
        listProp xs     = Nada `mappend` Only xs == Only xs
        prop (n, ns, s) = sumProp n && prodProp n && listProp ns && listProp s

propRightIdentity :: Property
propRightIdentity       = forAll gen prop
  where gen             = genTriple
        sumProp n       = Only (Sum n) `mappend` Nada == Only (Sum n)
        prodProp n      = Only (Product n) `mappend` Nada == Only (Product n)
        listProp xs     = Only xs `mappend` Nada == Only xs
        prop (n, ns, s) = sumProp n && prodProp n && listProp ns && listProp s

propAssociativity :: Property
propAssociativity          = forAll gen prop
  where gen                = do x  <- genInt
                                y  <- genInt
                                z  <- genInt
                                xs <- genListInt
                                ys <- genListInt
                                zs <- genListInt
                                s1 <- genString
                                s2 <- genString
                                s3 <- genString
                                return ((x, y, z), (xs, ys, zs), (s1, s2, s3))
        sumProp (x, y, z)  = (Only (Sum x) `mappend` Only (Sum y)) `mappend`
          Only (Sum z) == Only (Sum x) `mappend` (Only (Sum y) `mappend`
          Only (Sum z))
        prodProp (x, y, z) = (Only (Product x) `mappend` Only (Product y))
          `mappend` Only (Product z) == Only (Product x) `mappend`
          (Only (Product y) `mappend` Only (Product z))
        listProp (x, y, z) = (Only x `mappend` Only y) `mappend` Only z ==
          Only x `mappend` (Only y `mappend` Only z)
        prop (t1, t2, t3)  = sumProp t1 && prodProp t1 && listProp t2 &&
          listProp t3

runPropTests :: IO ()
runPropTests = mapM_ quickCheck [ propLeftIdentity
                                , propRightIdentity
                                , propAssociativity
                                ]

runAllTests :: IO ()
runAllTests = do runSpecTests
                 runPropTests

{-

Madness

You may have seen mad libs 1 before. The idea is to take a template
of phrases, fill them in with blindly selected categories of words, and
see if saying the final version is amusing.
Using an example from the Wikipedia article on Mad Libs:
"___________! he said ______ as he jumped into his car
 exclamation          adverb
____ and drove off with his _________ wife."
noun                        adjective

We can make this into a function, like the following:

-}

type Verb        = String
type Adjective   = String
type Adverb      = String
type Noun        = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj = e <> "! he said " <> adv <>
  " as he jumped into his car " <> noun <> " and drove off with this " <>
  adj <> " wife."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj = mconcat [ e
                                          , "! he said "
                                          , adv
                                          , " as he jumped into his car "
                                          , noun
                                          , " and drove off with this "
                                          , adj
                                          , " wife."
                                          ]

{-

Exercise: Maybe Another Monoid

Write a Monoid instance for Maybe type which doesn’t require a Monoid
for the contents. Reuse the Monoid law QuickCheck properties and
use them to validate the instance.

-}

monoidAssoc :: (Eq m, Monoid m, Semigroup m) => m -> m -> m -> Bool
monoidAssoc x y z = (x <> (y <> z)) == ((x <> y) <> z)

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdentity x = mempty <> x == x

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdentity x = x <> mempty == x

newtype First' a = First' { getFirst' :: Optional a }
                 deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
         (Only x) <> (Only y) = Only $ x <> y
         Nada <> x            = x
         x <> Nada            = x

instance Semigroup a => Semigroup (First' a) where
         (First' x) <> (First' y) = First' $ x <> y

instance Monoid a => Monoid (First' a) where
         mempty                          = First' Nada
         (First' x) `mappend` (First' y) = First' $ x `mappend` y

firstMappend :: Monoid a => First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

instance Arbitrary a => Arbitrary (Optional a) where
         arbitrary = do x <- arbitrary
                        elements [Only x, Nada]

instance Arbitrary a => Arbitrary (First' a) where
         arbitrary = do x <- arbitrary
                        return $ First' x

main                                      :: IO ()
main = do quickCheck (monoidAssoc         :: FirstMappend)
          quickCheck (monoidLeftIdentity  :: First' String -> Bool)
          quickCheck (monoidRightIdentity :: First' String -> Bool)

{-

Chapter exercises

Semigroup exercises

Given a datatype, implement the Semigroup instance. Add Semi-
group constraints to type variables where needed. Use the Semigroup
class from the semigroups library or write your own. When we use
<>, we mean the infix mappend from the Semigroup typeclass.

Note We’re not always going to derive every instance you may want
or need in the datatypes we provide for exercises. We expect you to
know what you need and to take care of it yourself by this point.

1. Validate all of your instances with QuickCheck. Since Semi-
group’s only law is associativity, that’s the only property you
need to reuse.

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
         _ <> _ = undefined

instance Arbitrary Trivial where
         arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

main                              :: IO ()
main = quickCheck (semigroupAssoc :: TrivialAssoc)

-}

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
         Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
         arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc x y z = (x <> (y <> z)) == ((x <> y) <> z)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

sgmain                                 :: IO ()
sgmain = do quickCheck (semigroupAssoc :: TrivialAssoc)
            quickCheck (semigroupAssoc :: IdentityAssoc)
            quickCheck (semigroupAssoc :: TwoAssoc)
            quickCheck (semigroupAssoc :: ThreeAssoc)
            quickCheck (semigroupAssoc :: BoolConjAssoc)
            quickCheck (semigroupAssoc :: BoolDisjAssoc)
            quickCheck (semigroupAssoc :: OrAssoc)
            quickCheck (semigroupAssoc :: ValidationAssoc)
            quickCheck (semigroupAssoc :: AccumulateRightAssoc)
            quickCheck (semigroupAssoc :: AccumulateBothAssoc)

{-

2.

newtype Identity a = Identity a

-}

newtype Identity a = Identity { runIdentity :: a }
                   deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
         (Identity x) <> (Identity y) = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
         arbitrary = do x <- arbitrary
                        return $ Identity x

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

{-

3.

data Two a b = Two a b

-}

data Two a b = Two a b
             deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
         (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
         arbitrary = do x <- arbitrary
                        y <- arbitrary
                        return $ Two x y

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

{-

4.

data Three a b c = Three a b c

-}

data Three a b c = Three a b c
                 deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
         (Three x y z) <> (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
         arbitrary = do x <- arbitrary
                        y <- arbitrary
                        z <- arbitrary
                        return $ Three x y z

type ThreeAssoc = Three String String String
 -> Three String String String
 -> Three String String String
 -> Bool

{-

5.

data Four a b c d = Four a b c d

-}

data Four a b c d = Four a b c d
                  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
         (Four w x y z) <> (Four w' x' y' z') = Four (w <> w') (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
         arbitrary = do w <- arbitrary
                        x <- arbitrary
                        y <- arbitrary
                        z <- arbitrary
                        return $ Four w x y z

{-

6.

newtype BoolConj = BoolConj Bool

What it should do:

Prelude> (BoolConj True) <> (BoolConj True)
BoolConj True

Prelude> (BoolConj True) <> (BoolConj False)
BoolConj False

-}

newtype BoolConj = BoolConj Bool
                 deriving (Eq, Show)

instance Semigroup BoolConj where
         BoolConj True <> BoolConj True = BoolConj True
         BoolConj False <> _            = BoolConj False
         _ <> BoolConj False            = BoolConj False

instance Arbitrary BoolConj where
         arbitrary = elements [BoolConj True, BoolConj False]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

{-

7.

newtype BoolDisj = BoolDisj Bool

What it should do:

Prelude> (BoolDisj True) <> (BoolDisj True)
BoolDisj True

Prelude> (BoolDisj True) <> (BoolDisj False)
BoolDisj True

-}

newtype BoolDisj = BoolDisj Bool
                 deriving (Eq, Show)

instance Semigroup BoolDisj where
         (BoolDisj False) <> (BoolDisj False) = BoolDisj False
         (BoolDisj True) <> _                 = BoolDisj True
         _ <> (BoolDisj True)                 = BoolDisj True

instance Arbitrary BoolDisj where
         arbitrary = elements [BoolDisj True, BoolDisj False]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

{-

8.

data Or a b = Fst a
            | Snd b

The Semigroup for Or should have the following behavior. We
can think of this as having a “sticky” Snd value where it’ll hold
onto the first Snd value when and if one is passed as an argument.
This is similar to the First' Monoid you wrote earlier.

Prelude> Fst 1 <> Snd 2
Snd 2

Prelude> Fst 1 <> Fst 2
Fst 2

Prelude> Snd 1 <> Fst 2
Snd 1

Prelude> Snd 1 <> Snd 2
Snd 1

-}

data Or a b = Fst a
            | Snd b
            deriving (Eq, Show)

instance Semigroup (Or a b) where
         (Fst _) <> (Fst x) = Fst x
         (Snd x) <> (Snd _) = Snd x
         (Fst _) <> (Snd x) = Snd x
         (Snd x) <> (Fst _) = Snd x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
         arbitrary = do x <- arbitrary
                        y <- arbitrary
                        elements [Fst x, Snd y]

type OrAssoc = Or String String -> Or String String -> Or String String -> Bool

{-

9.
newtype Combine a b = Combine { unCombine :: (a -> b) }

What it should do:

Prelude> let f = Combine $ \n -> Sum (n + 1)
Prelude> let g = Combine $ \n -> Sum (n - 1)

Prelude> unCombine (f <> g) $ 0
Sum {getSum = 0}

Prelude> unCombine (f <> g) $ 1
Sum {getSum = 2}

Prelude> unCombine (f <> f) $ 1
Sum {getSum = 4}

Prelude> unCombine (g <> f) $ 1
Sum {getSum = 2}

Hint: This function will eventually be applied to a single value
of type a. But you’ll have multiple functions that can produce a
value of type b. How do we combine multiple values so we have
a single b? This one will probably be tricky! Remember that the
type of the value inside of Combine is that of a function. If you
can’t figure out CoArbitrary, don’t worry about QuickChecking
this one.

-}

newtype Combine a b = Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
         (Combine f) <> (Combine g) = Combine $ \n -> f n <> g n

instance (Arbitrary a, CoArbitrary b) => CoArbitrary (Combine a b) where
         coarbitrary (Combine _) _ = undefined

{-

10.
newtype Comp a = Comp { unComp :: (a -> a) }

Hint: We can do something that seems a little more specific and
natural to functions now that the input and output types are the
same.

-}

newtype Comp a = Comp { unComp :: a -> a }

instance Semigroup a => Semigroup (Comp a) where
         (Comp x) <> (Comp y) = Comp $ x . y

{-

11.
-- Look familiar?

data Validation a b = Failure a | Success b
                    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
         (<>) = undefined

-}

data Validation a b = Failure a | Success b
                    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
         (Failure x) <> (Failure y) = Failure $ x <> y
         (Failure x) <> (Success _) = Failure x
         (Success _) <> (Failure x) = Failure x
         (Success _) <> (Success y) = Success y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
         arbitrary = do x <- arbitrary
                        y <- arbitrary
                        elements [Failure x, Success y]

type ValidationAssoc = Validation String Int
 -> Validation String Int
 -> Validation String Int
 -> Bool

{-

12.
-- Validation with a Semigroup
-- that does something different

newtype AccumulateRight a b = AccumulateRight (Validation a b)
                            deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
         (<>) = undefined

-}

newtype AccumulateRight a b = AccumulateRight (Validation a b)
                            deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
         (AccumulateRight (Success x)) <> (AccumulateRight (Success y)) =
           AccumulateRight $ Success $ x <> y
         (AccumulateRight (Success x)) <> (AccumulateRight (Failure _)) =
           AccumulateRight $ Success x
         (AccumulateRight (Failure _)) <> (AccumulateRight (Success x)) =
           AccumulateRight $ Success x
         (AccumulateRight (Failure _)) <> (AccumulateRight (Failure x)) =
           AccumulateRight $ Failure x

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
         arbitrary = do x <- arbitrary
                        y <- arbitrary
                        elements [ AccumulateRight $ Failure x
                                 , AccumulateRight $ Success y
                                 ]

type AccumulateRightAssoc = AccumulateRight Int String
 -> AccumulateRight Int String
 -> AccumulateRight Int String
 -> Bool

{-

13.
-- Validation with a Semigroup
-- that does something more

newtype AccumulateBoth a b = AccumulateBoth (Validation a b)
                           deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
         (<>) = undefined

-}

newtype AccumulateBoth a b = AccumulateBoth (Validation a b)
                           deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
         (AccumulateBoth (Success x)) <> (AccumulateBoth (Success y)) =
           AccumulateBoth $ Success $ x <> y
         (AccumulateBoth (Success x)) <> (AccumulateBoth (Failure _)) =
           AccumulateBoth $ Success x
         (AccumulateBoth (Failure _)) <> (AccumulateBoth (Success x)) =
           AccumulateBoth $ Success x
         (AccumulateBoth (Failure x)) <> (AccumulateBoth (Failure y)) =
           AccumulateBoth $ Failure $ x <> y

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
         arbitrary = do x <- arbitrary
                        y <- arbitrary
                        elements [ AccumulateBoth $ Failure x
                                 , AccumulateBoth $ Success y
                                 ]

type AccumulateBothAssoc = AccumulateBoth String String
 -> AccumulateBoth String String
 -> AccumulateBoth String String
 -> Bool

{-

Monoid exercises

Given a datatype, implement the Monoid instance. Add Monoid
constraints to type variables where needed. For the datatypes you’ve
already implemented Semigroup instances for, you just need to
figure out what the identity value is.

1. Again, validate all of your instances with QuickCheck. Example
scaffold is provided for the Trivial type.

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
         (<>) = undefined

instance Monoid Trivial where
mempty  = undefined
mappend = (<>)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

main                                      :: IO ()
main = do quickCheck (semigroupAssoc      :: TrivialAssoc)
          quickCheck (monoidLeftIdentity  :: Trivial -> Bool)
          quickCheck (monoidRightIdentity :: Trivial -> Bool)

-}

instance Monoid Trivial where
         mempty  = Trivial
         mappend = (<>)

mmain                                      :: IO ()
mmain = do quickCheck (semigroupAssoc      :: TrivialAssoc)
           quickCheck (monoidLeftIdentity  :: Trivial           -> Bool)
           quickCheck (monoidRightIdentity :: Trivial           -> Bool)
           quickCheck (semigroupAssoc      :: IdentityAssoc)
           quickCheck (monoidLeftIdentity  :: Identity String   -> Bool)
           quickCheck (monoidRightIdentity :: Identity String   -> Bool)
           quickCheck (semigroupAssoc      :: TwoAssoc)
           quickCheck (monoidLeftIdentity  :: Two String String -> Bool)
           quickCheck (monoidRightIdentity :: Two String String -> Bool)
           quickCheck (semigroupAssoc      :: BoolConjAssoc)
           quickCheck (monoidLeftIdentity  :: BoolConj          -> Bool)
           quickCheck (monoidRightIdentity :: BoolConj          -> Bool)
           quickCheck (semigroupAssoc      :: BoolDisjAssoc)
           quickCheck (monoidLeftIdentity  :: BoolDisj          -> Bool)
           quickCheck (monoidRightIdentity :: BoolDisj          -> Bool)

{-

2.

newtype Identity a = Identity a deriving Show

-}

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
         mempty  = Identity mempty
         mappend = (<>)

{-

3.

data Two a b = Two a b deriving Show

-}

instance (Monoid a, Monoid b, Semigroup a, Semigroup b) => Monoid (Two a b) where
         mempty  = Two mempty mempty
         mappend = (<>)

{-

4.

newtype BoolConj = BoolConj Bool

What it should do:

Prelude> (BoolConj True) `mappend` mempty
BoolConj True

Prelude> mempty `mappend` (BoolConj False)
BoolConj False

-}

instance Monoid BoolConj where
         mempty  = BoolConj True
         mappend = (<>)

{-

5.

newtype BoolDisj = BoolDisj Bool

What it should do:

Prelude> (BoolDisj True) `mappend` mempty
BoolDisj True

Prelude> mempty `mappend` (BoolDisj False)
BoolDisj False

-}

instance Monoid BoolDisj where
         mempty  = BoolDisj False
         mappend = (<>)

{-

6.
newtype Combine a b = Combine { unCombine :: (a -> b) }

What it should do:

Prelude> let f = Combine $ \n -> Sum (n + 1)
Prelude> unCombine (mappend f mempty) $ 1
Sum {getSum    = 2}

-}

instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
         mempty  = Combine $ const mempty
         mappend = (<>)

{-

7. Hint: We can do something that seems a little more specific and
natural to functions now that the input and output types are the
same.

newtype Comp a = Comp (a -> a)

-}

instance (Monoid a, Semigroup a) => Monoid (Comp a) where
         mempty  = Comp mempty
         mappend = (<>)
