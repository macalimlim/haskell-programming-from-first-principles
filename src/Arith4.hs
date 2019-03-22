{-# LANGUAGE NoImplicitPrelude #-}

module Arith4 where

import Data.Function ((.))
import GHC.Integer   (Integer)
import System.IO     (IO, print)
import Text.Read     (Read, read)
import Text.Show     (Show, show)

id :: a -> a
id x = x

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

main                          :: IO ()
main = do print (roundTrip (4 :: Integer) :: Integer)
          (print . id) (4     :: Integer)
