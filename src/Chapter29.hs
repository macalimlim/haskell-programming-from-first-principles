{-# LANGUAGE NoImplicitPrelude #-}

module Chapter29 where

import Cipher2            (cipher, crack)
import Control.Monad      (return)
import Data.Function      (flip, ($))
import Data.String        (String)
import Debug.Trace        (trace)
import GHC.Err            (error)
import System.Environment (getArgs)
import System.IO          (Handle, IO, IOMode (ReadMode, WriteMode), hClose,
                           hGetLine, hIsEOF, hPutStrLn, openFile, putStrLn)

{-

Chapter Exercises

File I/O with Vigenère

Reusing the Vigenère cipher you wrote back in algebraic datatypes
and wrote tests for in testing, make an executable that takes a key
and a mode argument. If the mode is -d the executable decrypts the
input from standard in and writes the decrypted text to standard out.
If the mode is -e the executable blocks on input from standard input
(stdin) and writes the encrypted output to stdout.
Consider this an opportunity to learn more about how file handles
and the following members of the base library work:

System.Environment.getArgs :: IO [String]
System.IO.hPutStr          :: Handle -> String -> IO ()
System.IO.hGetChar         :: Handle -> IO Char
System.IO.stdout           :: Handle
System.IO.stdin            :: Handle

Whatever OS you’re on, you’ll need to learn how to feed files as
input to your utility and how to redirect standard out to a file. Part
of the exercise is figuring this out for yourself. You’ll want to use
hGetChar more than once to accept a string which is encrypted or
decrypted.

Add timeouts to your utility

Use hWaitForInput to make your utility timeout if no input is provided
within a span of time of your choosing. You can make it an optional
command-line argument. Exit with a nonzero error code and an
error message printed to standard error (stderr) instead of stdout.

System.IO.hWaitForInput :: Handle -> Int -> IO Bool
System.IO.stderr        :: Handle

-}

debug :: (a -> String -> a)
debug = flip trace

redirectToFile :: String -> Handle -> Handle -> IO ()
redirectToFile mode inH outH = do
  b        <- hIsEOF inH
  if b
    then return ()
    else do
      line <- hGetLine inH
      hPutStrLn outH $ processMode mode line
      redirectToFile mode inH outH

processMode :: String -> (String -> String)
processMode "-d" = crack
processMode "-e" = cipher
processMode _    = error "invalid mode"

main :: IO ()
main = do
  args     <- getArgs
  case args of
    [mode, key]             -> putStrLn $ processMode mode key
    [mode, inFile, outFile] -> do
      inH  <- openFile inFile ReadMode
      outH <- openFile outFile WriteMode
      redirectToFile mode inH outH
      hClose inH
      hClose outH
    _                       -> error "invalid arguments"
  return ()

{-

Config directories

Reusing the INI parser from the Parsing chapter, parse a directory of
INI config files into a Map whose key is the filename and whose value
is the result of parsing the INI file. Only parse files in the directory
that have the file extension .ini.

-}
