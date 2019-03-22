{-# LANGUAGE NoImplicitPrelude #-}

module Chapter14 where

import Chapter8        (recMul)
import Control.Monad   (mapM_, return)
import Data.Bool       (Bool (False, True), (&&), (||))
import Data.Char       (toUpper)
import Data.Eq         (Eq, (/=), (==))
import Data.Foldable   (concat, foldr)
import Data.Function   (id, ($), (.))
import Data.Functor    (fmap)
import Data.Int        (Int)
import Data.List       (length, reverse, sort, take, (++))
import Data.Maybe      (Maybe (Just, Nothing))
import Data.Ord        (Ord, (>=))
import Data.String     (String)
import Data.Tuple      (snd)
import GHC.Integer     (Integer)
import GHC.Num         (Num, (*), (+))
import GHC.Real        (Fractional, div, mod, quot, rem, (/), (^))
import GHC.Types       (Double)
import System.IO       (IO)
import Test.Hspec      (describe, hspec, it, shouldBe)
import Test.QuickCheck (Arbitrary, Gen, Property, arbitrary, elements, forAll,
                        frequency, quickCheck, suchThat, verboseCheck)
import Text.Read       (read)
import Text.Show       (Show, show)
import WordNumber      (digitToWord, digits, wordNumber)

--deepCheck = quickCheckWith (stdArgs {maxSuccess = 1000})

{-

Intermission: Short Exercise

In the Chapter Exercises at the end of Recursion, you were given this
exercise:

Write a function that multiplies two numbers using recursive sum-
mation. The type should be (Eq a, Num a) => a -> a -> a although,
depending on how you do it, you might also consider adding an Ord
constraint.

If you still have your answer, great! If not, rewrite it and then
write Hspec tests for it.

The above examples demonstrate the basics of writing individual
tests to test particular values. If you’d like to see a more developed
example, you could refer to Chris’s library, Bloodhound. 3

-}

testRecMul :: IO ()
testRecMul = hspec $ describe "recMul" $ do it "recMul 5 1 is 5" $
                                              recMul 5 1 `shouldBe` 5
                                            it "recMul 5 4 is 20" $
                                              recMul 5 4 `shouldBe` 20

{-

Chapter Exercises

Now it’s time to write some tests of your own. You could write tests
for most of the exercises you’ve done in the book, but whether you’d
want to use Hspec or QuickCheck depends on what you’re trying to
test. We’ve tried to simplify things a bit by telling you which to use
for these exercises, but, as always, we encourage you to experiment
on your own.

Validating numbers into words

Remember the “numbers into words” exercise in Recursion? You’ll
be writing tests to validate the functions you wrote.

main :: IO ()
main = hspec $ do
  describe "digitToWord does what we want" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      print "???"
  describe "digits does what we want" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      print "???"
  describe "wordNumber does what we want" $ do
    it "returns one-zero-zero for 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "returns nine-zero-zero-one for 9001" $ do
      print "???"

Fill in the test cases that print question marks. If you think of
additional tests you could perform, add them.

-}

testHspec :: IO ()
testHspec = hspec $ do
  describe "digitToWord does what we want" $ do
    it "returns zero for 0" $
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $
      digitToWord 1 `shouldBe` "one"
    it "returns two for 2" $
      digitToWord 2 `shouldBe` "two"
    it "returns three for 3" $
      digitToWord 3 `shouldBe` "three"
    it "returns four for 4" $
      digitToWord 4 `shouldBe` "four"
    it "returns five for 5" $
      digitToWord 5 `shouldBe` "five"
    it "returns six for 6" $
      digitToWord 6 `shouldBe` "six"
    it "returns seven for 7" $
      digitToWord 7 `shouldBe` "seven"
    it "returns eight for 8" $
      digitToWord 8 `shouldBe` "eight"
    it "returns nine for 9" $
      digitToWord 9 `shouldBe` "nine"
  describe "digits does what we want" $ do
    it "returns [1] for 1" $
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $
      digits 100 `shouldBe` [1, 0, 0]
    it "returns [1, 2, 3] for 123" $
      digits 123 `shouldBe` [1, 2, 3]
  describe "wordNumber does what we want" $ do
    it "returns one-zero-zero for 100" $
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "returns nine-zero-zero-one for 9001" $
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"
    it "returns one for 1" $
      wordNumber 1 `shouldBe` "one"

{-

Using QuickCheck

Test some simple arithmetic properties using QuickCheck.

1.

-- for a function
half x = x / 2

-- this property should hold
halfIdentity = (*2) . half

-}

half :: Fractional a => a -> a
half x = x / 2

genDouble :: Gen Double
genDouble = arbitrary

propHalfIdentity :: Property
propHalfIdentity = forAll gen prop
  where gen      = genDouble
        prop x   = x == ((* 2) . half) x

propHalfEqualToPointFive :: Property
propHalfEqualToPointFive = forAll gen prop
  where gen              = genDouble
        prop x           = (x * 0.5) == half x

propHalfOneFourth :: Property
propHalfOneFourth = forAll gen prop
  where gen       = genDouble
        prop x    = (x * 0.25) == half (half x)

testPropsHalf :: IO ()
testPropsHalf = mapM_ quickCheck [ propHalfIdentity
                                 , propHalfEqualToPointFive
                                 , propHalfOneFourth
                                 ]

{-

2.
import Data.List (sort)

-- for any list you apply sort to
-- this property should hold

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs                 = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t)      = (Just y, t)
        go y (Just x, t)       = (Just y, x >= y)

-}

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs                 = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t)      = (Just y, t)
        go y (Just x, _)       = (Just y, x >= y)

genIntList :: Gen [Int]
genIntList = arbitrary

genIntegerList :: Gen [Integer]
genIntegerList = arbitrary

genString :: Gen String
genString = arbitrary

propOrdered :: (Arbitrary a, Ord a, Show a) => Gen [a] -> Property
propOrdered gen = forAll gen prop
  where prop xs = listOrdered $ sort xs

testPropOrdered :: IO ()
testPropOrdered = mapM_ quickCheck [ propOrdered genIntList
                                   , propOrdered genIntegerList
                                   , propOrdered genString
                                   ]

{-

3.
Now we’ll test the associative and commutative properties of
addition:

plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative x y = x + y == y + x

-}

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

gen3Nums :: (Arbitrary a, Num a) => Gen (a, a, a)
gen3Nums = do x <- arbitrary
              y <- arbitrary
              z <- arbitrary
              return (x, y, z)

gen2Nums :: (Arbitrary a, Num a) => Gen (a, a)
gen2Nums = do x <- arbitrary
              y <- arbitrary
              return (x, y)

gen3Ints :: Gen (Int, Int, Int)
gen3Ints = gen3Nums

gen2Ints :: Gen (Int, Int)
gen2Ints = gen2Nums

propPlusAssociative :: Property
propPlusAssociative    = forAll gen prop
  where gen            = gen3Ints
        prop (x, y, z) = plusAssociative x y z

propPlusCommutative :: Property
propPlusCommutative = forAll gen prop
  where gen         = gen2Ints
        prop (x, y) = plusCommutative x y

testAdditionProps :: IO ()
testAdditionProps = mapM_ quickCheck [ propPlusAssociative
                                     , propPlusCommutative
                                     ]

{-

4.

Now do the same for multiplication.

-}

multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multAssociative x y z = x + (y + z) == (x + y) + z

multCommutative :: (Eq a, Num a) => a -> a -> Bool
multCommutative x y = x + y == y + x

propMultAssociative :: Property
propMultAssociative    = forAll gen prop
  where gen            = gen3Ints
        prop (x, y, z) = multAssociative x y z

propMultCommutative :: Property
propMultCommutative = forAll gen prop
  where gen         = gen2Ints
        prop (x, y) = multCommutative x y

testMultProps :: IO ()
testMultProps = mapM_ quickCheck [propMultAssociative, propMultCommutative]

{-

5.
We mentioned in one of the first chapters that there are some
laws involving the relationship of quot and rem and div and mod.
Write QuickCheck tests to prove them.

-- quot rem

(quot x y) * y + (rem x y) == x

(div x y) * y + (mod x y) == x

-}

genInt :: Gen Int
genInt = arbitrary

genIntGteOne :: Gen Int
genIntGteOne = genInt `suchThat` (>= 1)

genNoLessThanOneInt :: Gen (Int, Int)
genNoLessThanOneInt = do x <- genIntGteOne
                         y <- genIntGteOne
                         return (x, y)

propQuotRem :: Property
propQuotRem         = forAll gen prop
  where gen         = genNoLessThanOneInt
        prop (x, y) = quot x y * y + rem x y == x

propDivMod :: Property
propDivMod          = forAll gen prop
  where gen         = genNoLessThanOneInt
        prop (x, y) = div x y * y + mod x y == x

testPropQuotRemDivMod :: IO ()
testPropQuotRemDivMod = mapM_ quickCheck [ propQuotRem
                                         , propDivMod
                                         ]

{-

6.
Is (^) associative? Is it commutative? Use QuickCheck to see if
the computer can contradict such an assertion.

-}

propExpAssociative :: Property
propExpAssociative     = forAll gen prop
  where gen            = do x <- genInt
                            y <- genIntGteOne
                            z <- genIntGteOne
                            return (x, y, z)
        prop (x, y, z) = x ^ (y ^ z) == (x ^ y) ^ z ||
                         x ^ (y ^ z) /= (x ^ y) ^ z

propExpCommutative :: Property
propExpCommutative  = forAll gen prop
  where gen         = do x <- genIntGteOne
                         y <- genIntGteOne
                         return (x, y)
        prop (x, y) = x ^ y == y ^ x || x ^ y /= y ^ x

testPropExp :: IO ()
testPropExp = mapM_ verboseCheck [ propExpAssociative
                                 , propExpCommutative
                                 ]

{-

7.
Test that reversing a list twice is the same as the identity of the
list:

reverse . reverse == id

-}

propReverseIdentity :: Property
propReverseIdentity = forAll gen prop
  where gen         = genIntList
        prop xs     = (reverse . reverse) xs == id xs

testPropReverse :: IO ()
testPropReverse = mapM_ quickCheck [propReverseIdentity]

{-

8.
Write a property for the definition of ($) and (.).

f $ a = f a

f . g = \x -> f (g x)

-}

propApplyIdentity :: Property
propApplyIdentity = forAll gen prop
  where gen       = genInt
        prop x    = id $ x == id x

propComposeIdentity :: Property
propComposeIdentity = forAll gen prop
  where gen         = genInt
        prop x      = (id . id) x == id (id x)

testPropApplyAndCompose :: IO ()
testPropApplyAndCompose = mapM_ quickCheck [ propApplyIdentity
                                           , propComposeIdentity
                                           ]

{-

9.
See if these two functions are equal:

foldr (:) == (++)

foldr (++) [] == concat

-}

genListIntsInts :: Gen [[Int]]
genListIntsInts = arbitrary

genListInts :: Gen [Int]
genListInts = arbitrary

propFoldrEqual :: Property
propFoldrEqual            = forAll gen prop
  where gen               = do xs <- genListIntsInts
                               ys <- genListInts
                               zs <- genListInts
                               return (xs, ys, zs)
        prop (xs, ys, zs) = foldr (++) [] xs == concat xs &&
                            foldr (:) ys zs == ys ++ zs

{-

10.
Hm. Is that so?

f n xs = length (take n xs) == n

-}

propLengthTake :: Property
propLengthTake       = forAll gen prop
  where gen          = do n  <- genInt
                          xs <- genListInts
                          return (n, xs)
        prop (n, xs) =  length (take n xs) == n

testPropLengthTake :: IO ()
testPropLengthTake = quickCheck propLengthTake

{-

11.
Finally, this is a fun one. You may remember we had you com-
pose read and show one time to complete a “round trip.” Well,
now you can test that it works:

f x = (read (show x)) == x

-}

propReadShow :: Property
propReadShow   = forAll gen prop
  where gen    = genInt
        prop n = read (show n) == n


{-

Failure

Find out why this property fails.

-- for a function
square x = x * x

-- why does this property not hold? Examine the type of sqrt.
squareIdentity = square . sqrt

Hint: Read about floating point arithmetic and precision if you’re
unfamiliar with it.

=> because of high precision and the large number of decimal places


Idempotence

Idempotence refers to a property of some functions in which the
result value does not change beyond the initial application. If you
apply the function once, it returns a result, and applying the same
function to that value won’t ever change it. You might think of a list
that you sort: once you sort it, the sorted list will remain the same
after applying the same sorting function to it. It’s already sorted, so
new applications of the sort function won’t change it.

Use QuickCheck and the following helper functions to demon-
strate idempotence for the following:

-}

twice :: (a -> a) -> (a -> a)
twice f = f . f

fourTimes :: (a -> a) -> (a -> a)
fourTimes = twice . twice

{-

1. f x = capitalizeWord x == twice capitalizeWord x ==
  fourTimes capitalizeWord x

-}

capitalize :: String -> String
capitalize = fmap toUpper

propCapitalizeIdempotence             :: Property
propCapitalizeIdempotence = forAll gen prop
  where gen               = arbitrary :: Gen String
        prop s            = c1 s && c2 s && c3 s
        c1 s              = capitalize s == twice capitalize s
        c2 s              = capitalize s == fourTimes capitalize s
        c3 s              = twice capitalize s == fourTimes capitalize s

{-

2. f x = sort x == twice sort x == fourTimes sort x

-}

propSortIdempotence             :: Property
propSortIdempotence = forAll gen prop
  where gen         = arbitrary :: Gen String
        prop s      = c1 s && c2 s && c3 s
        c1 s        = sort s == twice sort s
        c2 s        = sort s == fourTimes sort s
        c3 s        = twice sort s == fourTimes sort s

testPropTwice :: IO ()
testPropTwice = mapM_ verboseCheck [ propCapitalizeIdempotence
                                   , propSortIdempotence
                                   ]

{-

Make a Gen random generator for the datatype

We demonstrated in the chapter how to make Gen generators for
different datatypes. We are so certain you enjoyed that, we are going
to ask you to do it for some new datatypes:

1. Equal probabilities for each.

data Fool = Fulse
          | Frue
          deriving (Eq, Show)

-}

data Fool = Fulse
          | Frue
          deriving (Eq, Show)

instance Arbitrary Fool where
  arbitrary = elements [Fulse, Frue]

{-

2. 2/3s chance of Fulse, 1/3 chance of Frue.

data Fool = Fulse
          | Frue
          deriving (Eq, Show)

-}

data Fool' = Fulse'
           | Frue'
           deriving (Eq, Show)

instance Arbitrary Fool' where
  arbitrary = frequency [(2, return Fulse'), (1, return Frue')]

{-

Hangman testing

Next, you should go back to the Hangman project from the pre-
vious chapter and write tests. The kinds of tests you can write at
this point will be limited due to the interactive nature of the game.
However, you can test the functions. Focus your attention on testing
the following:

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
    where zipper guessed wordChar guessChar = if wordChar == guessed
                                                then Just wordChar
                                                else guessChar
          newFilledInSoFar                  = zipWith (zipper c) word filledInSoFar

and:

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True)         -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _)         -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _)        -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)

Refresh your memory on what those are supposed to do and then
test to make sure they do.

-}
