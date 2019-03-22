{-# LANGUAGE NoImplicitPrelude #-}

module Sing where

import Data.List   ((++))
import Data.Ord    ((>))
import Data.String (String)

fstString :: String -> String
fstString x = x ++ " in the rain"

sndString :: String -> String
sndString x = x ++ " over the rainbow"

sing :: String
sing      = if x > y then fstString x else sndString y
  where x = "Singing"
        y = "Somewhere"
