{-# LANGUAGE NoImplicitPrelude #-}

module Addition where

import Control.Monad   (return)
import Data.Bool       (Bool (False, True), otherwise)
import Data.Char       (Char)
import Data.Either     (Either (Left, Right))
import Data.Eq         ((==))
import Data.Function   (($))
import Data.Int        (Int)
import Data.Maybe      (Maybe (Just, Nothing))
import Data.Ord        (Ordering (EQ, GT, LT), (<), (>))
import GHC.Num         ((+), (-))
import GHC.Real        (Integral)
import System.IO       (IO, putStrLn)
import Test.Hspec      (describe, hspec, it, shouldBe)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, choose, elements, frequency,
                        property, quickCheck)

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom              = go num denom 0
  where go n d count | n < d     = (count, n)
                     | otherwise = go (n - d) d (count + 1)

genOne :: Gen Int
genOne = return 1

genOneThroughThree :: Gen Int
genOneThroughThree = elements [1, 2, 3]

oneThroughThree' :: Gen Int
oneThroughThree' = elements [1, 2, 2, 3, 3, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do x <- arbitrary
              y <- arbitrary
              return (x, y)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do x <- arbitrary
                 y <- arbitrary
                 z <- arbitrary
                 return (x, y, z)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do x <- arbitrary
               y <- arbitrary
               elements [Left x, Right y]

genMaybe :: (Arbitrary a) => Gen (Maybe a)
genMaybe = do x <- arbitrary
              elements [Nothing, Just x]

genMaybe' :: (Arbitrary a) => Gen (Maybe a)
genMaybe' = do x <- arbitrary
               frequency [(1, return Nothing), (3, return $ Just x)]

propAdditionGreater :: Int -> Bool
propAdditionGreater x = x + 1 > x

propAdditionIdentity :: Int -> Bool
propAdditionIdentity x = x + 0 == x

propAdditionFalse :: Int -> Bool
propAdditionFalse x = x + 0 > x

runQuickCheck :: IO ()
runQuickCheck = quickCheck propAdditionGreater

main                                                                  :: IO ()
main = hspec $ describe "Addition" $ do it "1 + 1 is greater than 1" $
                                          (1 + 1) > 1 `shouldBe` True
                                        it "2 + 2 is equal to 4" $
                                          2 + 2 `shouldBe` 4
                                        it "15 divided by 3 is 5" $
                                          dividedBy 15 3 `shouldBe` (5, 0)
                                        it "22 divided by 5 is 4 remainder 2" $
                                          dividedBy 22 5 `shouldBe` (4, 2)
                                        it "x + 1 is always greater than x" $
                                          property $ \x -> x + 1 > (x :: Int)
