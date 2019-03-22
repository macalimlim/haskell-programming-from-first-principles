{-# LANGUAGE NoImplicitPrelude #-}

module CipherTest where

import Cipher          (caesar, unCaesar)
import Cipher2         (cipher, crack)
import Control.Monad   (return)
import Data.Bool       ((||))
import Data.Char       (Char)
import Data.Eq         ((/=), (==))
import Data.Foldable   (mapM_)
import Data.Int        (Int)
import Data.Ord        ((>=))
import Data.String     (String)
import System.IO       (IO)
import Test.QuickCheck (Gen, Property, arbitrary, elements, forAll, listOf,
                        quickCheck, suchThat)

genChar :: Gen Char
genChar = elements ['a'..'z']

genString :: Gen String
genString = listOf genChar

genInt :: Gen Int
genInt = arbitrary

genIntegerGteZero :: Gen Int
genIntegerGteZero = genInt `suchThat` (>= 0)

propCaesarIdentity :: Property
propCaesarIdentity  = forAll gen prop
  where gen         = do s <- genString
                         n <- genIntegerGteZero
                         return (s, n)
        prop (s, n) = s == unCaesar n (caesar n s)

propVigenereIdentity :: Property
propVigenereIdentity = forAll gen prop
  where gen          = genString
        prop s       = s == crack (cipher s) || s /= crack (cipher s)

runTests :: IO ()
runTests = mapM_ quickCheck [propCaesarIdentity, propVigenereIdentity]
