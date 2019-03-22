{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Chapter24 where

import Control.Applicative     ((<|>))
import Control.Monad           (fail, mapM, return, (>>))
import Data.Bool               (Bool (False, True), otherwise)
import Data.Char               (Char)
import Data.Either             (Either (Left, Right))
import Data.Eq                 (Eq, (/=), (==))
import Data.Function           (const, ($), (.))
import Data.Functor            ((<$>))
import Data.Int                (Int)
import Data.List               (concat, drop, filter, foldl, foldr, iterate,
                                length, lines, reverse, sum, take, zipWith,
                                (++))
import Data.Maybe              (fromMaybe)
import Data.Monoid             (mempty)
import Data.Ord                (Ord)
import Data.Ratio              (Rational, (%))
import Data.String             (String)
import Data.Time               (Day, fromGregorianValid)
import Data.Word
import GHC.Err                 (error)
import GHC.Integer             (Integer)
import GHC.Num                 ((*), (+))
import GHC.Real                (fromIntegral, toRational, (^))
import System.IO               (IO, print, putStrLn)
import Text.Parser.Combinators (eof, skipOptional)
import Text.RawString.QQ       (r)
import Text.Read               (read)
import Text.Show               (Show, show)
import Text.Trifecta           (Parser, char, decimal, integer, letter, many,
                                oneOf, parseString, satisfy, skipMany, some,
                                string, unexpected)
import Text.Trifecta.Result    (Result (Success))

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser Char
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL :: String -> IO ()
pNL s = putStrLn ('\n' : s)

main :: IO ()
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'

{-

Exercises: Parsing Practice

1. There’s a combinator that’ll let us mark that we expect an input
stream to be “finished” at a particular point in our parser. In
the parsers library this is simply called eof (end-of-file) and is
in the Text.Parser.Combinators module. See if you can make the
one and oneTwo parsers fail because they didn’t exhaust the input
stream!

-}
oneEof :: Parser Char
oneEof = eof >> one

oneTwoEof :: Parser Char
oneTwoEof = eof >> oneTwo

{-

2. Use string to make a Parser that parses “1”, “12”, and “123” out of
the example input respectively. Try combining it with stop too.
That is, a single parser should be able to parse all three of those
strings.

-}
oneS :: Parser String
oneS = string "1"

oneTwoS :: Parser String
oneTwoS = string "12"

oneTwoThreeS :: Parser String
oneTwoThreeS = string "123"

oneS' :: Parser String
oneS' = oneS >> stop

oneTwoS' :: Parser String
oneTwoS' = oneTwoS >> stop

oneTwoThreeS' :: Parser String
oneTwoThreeS' = oneTwoThreeS >> stop

testParse' :: Parser String -> IO ()
testParse' p = print $ parseString p mempty "123"

{-

3. Try writing a Parser that does what string does, but using char.

-}
mapMChar :: String -> Parser String
mapMChar = mapM char

oneSC :: Parser String
oneSC = mapMChar "1"

oneTwoSC :: Parser String
oneTwoSC = mapMChar "12"

oneTwoThreeSC :: Parser String
oneTwoThreeSC = mapMChar "123"

oneSC' :: Parser String
oneSC' = oneSC >> stop

oneTwoSC' :: Parser String
oneTwoSC' = oneTwoSC >> stop

oneTwoThreeSC' :: Parser String
oneTwoThreeSC' = oneTwoThreeSC >> stop

{-

Exercise: Unit of Success

This should not be unfamiliar at this point, even if you do not understand
all the details:

Prelude> parseString integer mempty "123abc"
Success 123

Prelude> parseString (integer >> eof) mempty "123abc"
Failure (interactive):1:4: error: expected: digit,
end of input
123abc<EOF>
   ^

Prelude> parseString (integer >> eof) mempty "123"
Success ()

You may have already deduced why haskell returns () as a Success
result here; it’s consumed all the input but there is no result to return
from having done so. The result Success () tells you the parse
was successful and consumed the entire input, so there’s nothing to
return.

What we want you to try now is rewriting the final example so
it returns the integer that it parsed instead of Success (). It should
return the integer successfully when it receives an input with an
integer followed by an EOF and fail in all other cases:

Prelude> parseString (yourFuncHere) mempty "123"
Success 123

Prelude> parseString (yourFuncHere) mempty "123abc"
Failure (interactive):1:4: error: expected: digit,
end of input
123abc<EOF>
   ^

-}
yourFuncHere :: Parser Integer
yourFuncHere = do
  a <- integer
  eof
  return a

{-

Exercise: Try Try

Make a parser, using the existing fraction parser plus a new decimal
parser, that can parse either decimals or fractions. You’ll want to use
<|> from Alternative to combine the... alternative parsers. If you find
this too difficult, write a parser that parses straightforward integers
or fractions. Make a datatype that contains either an integer or a
rational and use that datatype as the result of the parser. Or use
Either. Run free, Inbox; Sent Items
grasshopper.

Hint: we’ve not explained it yet, but you may want to try try.

-}
parseFraction :: Parser Rational
parseFraction = do
  n <- decimal
  _ <- char '/'
  d <- decimal
  case d of
    0 -> fail "denominator cannot be zero"
    _ -> return $ n % d

parseDecimal :: Parser Rational
parseDecimal = do
  bd <- decimal
  _  <- char '.'
  ad <- decimal
  return $ f bd ad
  where
    f        :: Integer -> Integer -> Rational
    f x y    = toRational $ x + y

parseInteger :: Parser Integer
parseInteger = integer

parseRationalOnly :: Parser Rational
parseRationalOnly = parseFraction <|> parseDecimal

type RationalOrInteger = Either Rational Integer

parseRationalOrInteger :: Parser RationalOrInteger
parseRationalOrInteger = (Left <$> parseFraction) <|> (Right <$> parseInteger)

{-

Chapter Exercises

1. Write a parser for semantic versions as defined by http://semver.org/.
After making a working parser, write an Ord instance for the SemVer type
that obeys the specification outlined on the SemVer website.

Expected results:

Prelude> parseString parseSemVer mempty "2.1.1"
Success (SemVer 2 1 1 [] [])

Prelude> parseString parseSemVer mempty "1.0.0-x.7.z.92"
Success (SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] [])

Prelude> SemVer 2 1 1 [] [] > SemVer 2 1 0 [] []
True
-}

data NumberOrString = NOSS String
                    | NOSI Integer
                    deriving (Eq, Ord, Show)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata
            deriving (Eq, Ord, Show)

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = skipMany (oneOf ".") >> (NOSI <$> decimal) <|> (NOSS <$> some letter)

parseSemVer :: Parser SemVer
parseSemVer = do
  major    <- decimal
  minor    <- char '.' >> decimal
  patch    <- char '.' >> decimal
  release  <- many (oneOf "-") >> many parseNumberOrString
  metadata <- many (oneOf "-") >> many parseNumberOrString
  return $ SemVer major minor patch release metadata

{-

2. Write a parser for positive integer values. Don’t reuse the pre-existing
digit or integer functions, but you can use the rest of the libraries we’ve
shown you so far. You are not expected to write a parsing library from scratch.

Expected results:

Prelude> parseString parseDigit mempty "123"
Success '1'

Prelude> parseString parseDigit mempty "abc"
Failure (interactive):1:1: error: expected: parseDigit
abc<EOF>
^

Prelude> parseString base10Integer mempty "123abc"
Success 123

Prelude> parseString base10Integer mempty "abc"
Failure (interactive):1:1: error: expected: integer
abc<EOF>
^


Hint: Assume you’re parsing base-10 numbers. Use arithmetic
as a cheap “accumulator” for your final number as you parse
each digit left-to-right.

-}

digits :: String
digits = ['0'..'9']

parseDigit :: Parser Char
parseDigit    = foldl f (char '0') digits
  where f x y = x <|> char y

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

{-

3. Extend the parser you wrote to handle negative and positive
integers. Try writing a new parser in terms of the one you
already have to do this.

Prelude> parseString base10Integer' mempty "-123abc"
Success (-123)

-}
base10Integer' :: Parser Integer
base10Integer' = do
  sign  <- many $ oneOf "-"
  case sign of
    ['-'] -> do
      n <- base10Integer
      return $ n * (-1)
    _     -> base10Integer

{-

4. Write a parser for US/Canada phone numbers with varying
formats.

With the following behavior:

Prelude> parseString parsePhone mempty "123-456-7890"
Success (PhoneNumber 123 456 7890)

Prelude> parseString parsePhone mempty "1234567890"
Success (PhoneNumber 123 456 7890)

Prelude> parseString parsePhone mempty "(123) 456-7890"
Success (PhoneNumber 123 456 7890)

Prelude> parseString parsePhone mempty "1-123-456-7890"
Success (PhoneNumber 123 456 7890)

Cf. Wikipedia’s article on “National conventions for writing
telephone numbers”. You are encouraged to adapt the exercise
to your locality’s conventions if they are not part of the NNAP
scheme.

-}

-- aka area code
type NumberingPlanArea = Integer

type Exchange = Integer

type LineNumber = Integer

data PhoneNumber =
  PhoneNumber NumberingPlanArea
              Exchange
              LineNumber
  deriving (Eq, Show)

createPhoneNumber :: String -> String -> String -> PhoneNumber
createPhoneNumber npa ex ln = PhoneNumber (read npa) (read ex) (read ln)

parsePhone :: Parser PhoneNumber
parsePhone               = do
  pn      <- many parseDigit
  case length pn of
    0  -> do
      npa <- char '(' >> some parseDigit
      ex  <- char ')' >> char ' ' >> some parseDigit
      ln  <- parseHyphenThenDigit
      return $ createPhoneNumber npa ex ln
    1  -> do
      npa <- parseHyphenThenDigit
      ex  <- parseHyphenThenDigit
      ln  <- parseHyphenThenDigit
      return $ createPhoneNumber npa ex ln
    3  -> do
      ex  <- parseHyphenThenDigit
      ln  <- parseHyphenThenDigit
      return $ createPhoneNumber pn ex ln
    10 -> return $ createPhoneNumber (take 3 pn) (take 3 $ drop 3 pn) (take 4 $ drop 6 pn)
    _  -> return $ PhoneNumber 0 0 0
  where
    parseHyphenThenDigit = char '-' >> some parseDigit

{-

5. Write a parser for a log file format and sum the time spent in
each activity. Additionally, provide an alternative aggregation
of the data that provides average time spent per activity per day.
The format supports the use of comments which your parser
will have to ignore. The # characters followed by a date mark
the beginning of a particular day.

Log format example:

-}
content :: String
content =
  [r|

-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep

|]

{-

You are to derive a reasonable datatype for representing this
data yourself. For bonus points, make this bi-directional by
making a Show representation for the datatype which matches
the format you are parsing. Then write a generator for this data
using QuickCheck’s Gen and see if you can break your parser
with QuickCheck.

-}

type Task   = String
type Hour   = Int
type Minute = Int

data Line = Activity Hour Minute Task
          | ItineraryDate Day
          | Blank
          deriving (Eq, Ord, Show)

type Lines = [Line]

parseComment :: Parser Line
parseComment = do
  _ <- string "-- " >> many (satisfy $ const True)
  return Blank

parseMonthOrDay :: Parser Int
parseMonthOrDay = fromIntegral <$> decimal

parseItineraryDate :: Parser Line
parseItineraryDate = do
  _ <- string "# "
  y <- decimal
  m <- char '-' >> parseMonthOrDay
  d <- char '-' >> parseMonthOrDay
  _ <- skipOptional parseComment
  return $ ItineraryDate $ fromMaybe (error "invalid day") $ fromGregorianValid y m d

putZeroOrNot :: String -> String
putZeroOrNot x | length x == 1 = "0" ++ x
               | otherwise     = x

hours :: [String]
hours = putZeroOrNot . show <$> [0..23]

parseHour :: Parser Int
parseHour     = read <$> foldl f (string "00") hours
  where f x y = x <|> string y

minutes :: [String]
minutes = putZeroOrNot . show <$> [0..59]

parseMinute :: Parser Int
parseMinute   = read <$> foldl f (string "00") minutes
  where f x y = x <|> string y

parseSentence :: Parser String
parseSentence = many $ letter <|> char ' ' <|> char ','  <|> char '?'

parseActivity :: Parser Line
parseActivity = do
  h <- parseHour
  _ <- char ':'
  m <- parseMinute
  t <- char ' ' >> parseSentence
  _ <- skipOptional parseComment
  return $ Activity h m t

parseBlankLine :: Parser Line
parseBlankLine = do
  _ <- string ""
  return Blank

parseContent :: String -> [Line]
parseContent s                 = ls
 where ss                      = filter (/= "") $ lines s
       ls                      = h <$> filter g (f <$> ss)
       f                       = parseString (parseComment <|> parseItineraryDate <|> parseActivity) mempty
       g (Success Activity {}) = True
       g _                     = False
       h (Success x)           = x
       h _                     = error "error in parsing"

{-

6. Write a parser for IPv4 addresses.

-}

newtype IPAddress = IPAddress Word32
                  deriving (Eq, Ord)

{-

A 32-bit word is a 32-bit unsigned int. Lowest value is 0 rather
than being capable of representing negative numbers, but the
highest possible value in the same number of bits is twice as
high. Note:

Prelude> import Data.Int
Prelude> import Data.Word
Prelude> maxBound :: Int32
2147483647
Prelude> maxBound :: Word32
4294967295
Prelude> div 4294967295 2147483647
2

Word32 is an appropriate and compact way to represent IPv4 addresses.
You are expected to figure out not only how to parse the
typical IP address format, but how IP addresses work numerically
insofar as is required to write a working parser. This will
require using a search engine unless you have an appropriate
book on internet networking handy.

Example IPv4 addresses and their decimal representations:

172.16.254.1 -> 2886794753
204.120.0.15 -> 3430416399

-}

ipv4Range :: [String]
ipv4Range = show <$> [0..255]

parseRange :: [String] -> Parser String
parseRange    = foldr f (string "0")
  where f i s = s <|> string i

parseByte :: Parser Word32
parseByte = read <$> parseRange ipv4Range

parseIPv4Address :: Parser IPAddress
parseIPv4Address = do
  first  <- parseByte
  _      <- char '.'
  second <- parseByte
  _      <- char '.'
  third  <- parseByte
  _      <- char '.'
  fourth <- parseByte
  return $ IPAddress (first * 256 ^ 3 + second * 256 ^ 2 + third * 256 ^ 1 + fourth)

{-

7. Same as before, but IPv6.

-}

newtype IPAddress6 = IPAddress6 Word64
                   deriving (Eq, Ord)

{-

Example IPv6 addresses and their decimal representations:

0:0:0:0:0:ffff:ac10:be01                -> 281473568538113
0:0:0:0:0:ffff:cc78:f                   -> 281474112159759
FE80:0000:0000:0000:0202:B3FF:FE1E:8329 -> 338288524927261089654163772891438416681
2001:DB8::8:800:200C:417A               -> 42540766411282592856906245548098208122

One of the trickier parts about IPv6 will be full vs. collapsed
addresses and the abbrevations. See this Q&A thread13 about
IPv6 abbreviations for more.

Ensure you can parse abbreviated variations of the earlier examples like:

FE80::0202:B3FF:FE1E:8329
2001:DB8::8:800:200C:417A

-}

hexToNum :: String -> Word64
hexToNum "0" = 0
hexToNum "1" = 1
hexToNum "2" = 2
hexToNum "3" = 3
hexToNum "4" = 4
hexToNum "5" = 5
hexToNum "6" = 6
hexToNum "7" = 7
hexToNum "8" = 8
hexToNum "9" = 9
hexToNum "A" = 10
hexToNum "a" = 10
hexToNum "B" = 11
hexToNum "b" = 11
hexToNum "C" = 12
hexToNum "c" = 12
hexToNum "D" = 13
hexToNum "d" = 13
hexToNum "E" = 14
hexToNum "e" = 14
hexToNum "F" = 15
hexToNum "f" = 15
hexToNum _   = error "unknown hex"

zeroToNine :: String
zeroToNine = concat $ show <$> [0..9]

smallAToF :: String
smallAToF = ['a'..'f']

bigAToF :: String
bigAToF = ['A'..'F']

ipv6Range :: String
ipv6Range = zeroToNine ++ smallAToF ++ bigAToF

parseIPv6Range :: String -> Parser String
parseIPv6Range = foldr f (string "0")
  where f i s  = s <|> string [i]

base16List :: [Word64]
base16List = reverse $ iterate (16 *) 1

toDecimal :: [Word64] -> Word64
toDecimal ns = sum $ zipWith (*) ns base16List

parseHex :: Parser Word64
parseHex = hexToNum <$> parseIPv6Range ipv6Range

parseIPv6Address :: Parser IPAddress6
parseIPv6Address = do
  first1   <- parseHex
  first2   <- parseHex
  first3   <- parseHex
  first4   <- parseHex
  _        <- char ':'
  second1  <- parseHex
  second2  <- parseHex
  second3  <- parseHex
  second4  <- parseHex
  _        <- char ':'
  third1   <- parseHex
  third2   <- parseHex
  third3   <- parseHex
  third4   <- parseHex
  _        <- char ':'
  fourth1  <- parseHex
  fourth2  <- parseHex
  fourth3  <- parseHex
  fourth4  <- parseHex
  _        <- char ':'
  fifth1   <- parseHex
  fifth2   <- parseHex
  fifth3   <- parseHex
  fifth4   <- parseHex
  _        <- char ':'
  sixth1   <- parseHex
  sixth2   <- parseHex
  sixth3   <- parseHex
  sixth4   <- parseHex
  _        <- char ':'
  seventh1 <- parseHex
  seventh2 <- parseHex
  seventh3 <- parseHex
  seventh4 <- parseHex
  _        <- char ':'
  eightth1 <- parseHex
  eightth2 <- parseHex
  eightth3 <- parseHex
  eightth4 <- parseHex
  let first      = toDecimal [first1, first2, first3, first4]
      second     = toDecimal [second1, second2, second3, second4]
      third      = toDecimal [third1, third2, third3, third4]
      fourth     = toDecimal [fourth1, fourth2, fourth3, fourth4]
      fifth      = toDecimal [fifth1, fifth2, fifth3, fifth4]
      sixth      = toDecimal [sixth1, sixth2, sixth3, sixth4]
      seventh    = toDecimal [seventh1, seventh2, seventh3, seventh4]
      eightth    = toDecimal [eightth1, eightth2, eightth3, eightth4]
  return $ IPAddress6 (first + second + third + fourth + fifth + sixth + seventh + eightth)

{-

8. Remove the derived Show instances from the IPAddress/IPAd-
dress6 types, and write your own Show instance for each type
that renders in the typical textual format appropriate to each.

-}

instance Show IPAddress where
  show (IPAddress n) = "IPAddress " ++ show n

instance Show IPAddress6 where
  show (IPAddress6 n) = "IPAddress6 " ++ show n


{-

9. Write a function that converts between IPAddress and IPAd-
dress6.

-}

{-

10. Write a parser for the DOT language14 that Graphviz uses to
express graphs in plain-text.
We suggest you look at the AST datatype in Haphviz15 for ideas
on how to represent the graph in a Haskell datatype. If you’re
feeling especially robust, you can try using fgl16.

-}
