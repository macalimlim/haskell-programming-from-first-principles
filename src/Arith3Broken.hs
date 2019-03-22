{-# LANGUAGE NoImplicitPrelude #-}

module Arith3Broken where

import GHC.Num   (negate, (+))
import System.IO (IO, print)

main :: IO ()
main         = do print (1 + 2)
                  print 10
                  print (negate (-1))
                  print ((+) 0 blah)
  where blah = negate 1
