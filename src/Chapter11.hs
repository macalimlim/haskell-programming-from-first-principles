{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Chapter11 where

import Data.Bool       (Bool (False, True), otherwise, (&&))
import Data.Char
import Data.Either     (Either (Left, Right))
import Data.Eq         (Eq, (/=), (==))
import Data.Foldable   (Foldable)
import Data.Function   (($), (.))
import Data.Int
import Data.List       (concat, concatMap, elem, filter, foldr, init, length,
                        map, maximumBy, words, (++))
import Data.List.Split
import Data.Ord        (compare, (>))
import Data.String     (String)
import Data.Tuple      (fst)
import GHC.Enum        (Bounded, Enum, maxBound, minBound, succ)
import GHC.Err         (error)
import GHC.Integer     (Integer)
import GHC.Num         (Num, (*), (+))
import System.IO       (IO, print, putStrLn)
import Text.Show       (Show, show)

data Doggies a = Husky a
               | Mastiff a
               deriving (Eq, Show)

data DogueDeBordeaux a = DogueDeBordeaux a

{-

Intermission: Exercises

Given the datatypes defined in the above sections,

1. Is Doggies a type constructor or a data constructor?

=> type contstructor


2. What is the kind of Doggies ?

=> * -> *


3. What is the kind of Doggies String ?

=> *


4. What is the type of Husky 10 ?

=> Num a => Doggies a


5. What is the type of Husky (10 :: Integer) ?

=> Doggies Integer


6. What is the type of Mastiff "Scooby Doo" ?

=> Doggies String


7. Is DogueDeBordeaux a type constructor or a data constructor?

=> both type constructor and data constructor


8. What is the type of DogueDeBordeaux ?

=> a -> DogueDeBordeaux a


9. What is the type of DogueDeBordeaux "doggie!"

=> DogueDeBordeaux String

-}

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini
                  | Mazda
                  | Tata
                  deriving (Eq, Show)

data Airline = PapuAir
             | CatapultsRUs
             | TakeYourChancesUnited
             deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline
             deriving (Eq, Show)

{-

Intermission: Exercises

For these exercises, we’ll use the datatypes defined in the above section.
It would be good if you’d typed them all into a source file already, but
if you hadn’t please do so now. You can then define some sample data
on your own, or use these to get you started:

-}

myCar :: Vehicle
myCar = Car Mini (Price 14000)

urCar :: Vehicle
urCar = Car Mazda (Price 20000)

clownCar :: Vehicle
clownCar = Car Tata (Price 7000)

doge :: Vehicle
doge = Plane PapuAir

{-

1. What is the type of myCar ?
=> Vehicle


2. Given the following, define the functions:

isCar :: Vehicle -> Bool
isCar = undefined

isPlane :: Vehicle -> Bool
isPlane = undefined

areCars :: [Vehicle] -> [Bool]
areCars = undefined

-}

isCar :: Vehicle -> Bool
isCar Car{} = True
isCar _     = False

isPlane :: Vehicle -> Bool
isPlane Plane{} = True
isPlane _       = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

{-

3. Now we’re going to write a function to tell us the manufacturer
of a piece of data:

getManu :: Vehicle -> Manufacturer
getManu = undefined

-}

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu Plane{}   = error "no manufacturer"

{-

4. Given that we’re returning the Manufacturer , what will happen
if you use this on Plane data?
=> error!


5. All right. Let’s say you’ve decided to add the size of the plane as an
argument to the Plane constructor. Add that to your datatypes
in the appropriate places and change your data and functions
appropriately.

-}

data Vehicle' = Car' Manufacturer Price
              | Plane' Airline Integer
              deriving (Eq, Show)

doge' :: Vehicle'
doge' = Plane' PapuAir 1000

isPlane' :: Vehicle' -> Bool
isPlane' Plane'{} = True
isPlane' _        = False

getManu' :: Vehicle' -> Manufacturer
getManu' (Car' m _) = m
getManu' Plane'{}   = error "no manufacturer"

{-

Intermission: Exercises

While we haven’t explicitly described the rules for calculating the
cardinality of datatypes yet, you might already have an idea of how
to do it for simple datatypes with nullary constructors. Try not to
overthink these exercises – you can probably intuitively grasp what
the cardinality is based just on what you know.

1. data PugType = PugData

=> 1


2. For this one, recall that Bool is also defined with the | :

data Airline = PapuAir
             | CatapultsR'Us
             | TakeYourChancesUnited

=> 3


3. Given what we know about Int8 , what’s the cardinality of Int16 ?

=> lower bound is -32768 and upper bound is 32767


4. Use the REPL and maxBound and minBound to examine Int and
Integer . What can you say about the cardinality of those types?

=> for Int lower bound is -9223372036854775808
=> and upper bound is 9223372036854775807

=> for Integer lower bound and upper bound is none


5. Extra credit (impress your friends!): What’s the connection be-
tween the 8 in Int8 and that type’s cardinality of 256?

=> 2 ^ 8


Intermission: Exercises

-}

data Example = MakeExample deriving Show

{-

1. You can query the type of a value in GHCi with the :type com-
mand, also abbreviated :t . Example:

Prelude> :t False
False :: Bool

What is the type of data constructor MakeExample ? What hap-
pens when you request the type of Example ?

=> MakeExample :: Example
=> error not a data constructor


2. What if you try :info on Example in GHCi? Can you determine
what typeclass instances are defined for the Example type using
:info in GHCi?

=> it shows the type and data constructor and 1 instance (in this case Show)
=> yes


3. Try making a new datatype like Example but with a single type
argument added to MakeExample , such as Int . What has changed
when you query MakeExample with :type in GHCi?

-}

data Example' = MakeExample' Integer deriving Show

-- MakeExample' :: Integer -> Example'


{-

Intermission: Exercises

1. Reusing the TooMany typeclass, write an instance of the typeclass
for the type (Int, String) . This will require adding a language
pragma named FlexibleInstances if you do not use a newtype —
GHC will tell you what to do.

-}

class TooMany a where
      tooMany :: a -> Bool

instance TooMany Int where
         tooMany n = n > 42

instance TooMany (Int, String) where
         tooMany (n, _) = tooMany n

{-

2. Make another TooMany instance for (Int, Int) . Sum the values
together under the assumption this is a count of goats from two
fields.

-}

instance TooMany (Int, Int) where
         tooMany (n, n') = tooMany $ n + n'

{-

3. Make another TooMany instance, this time for (Num a, TooMany
a) => (a, a) . This can mean whatever you want, such as sum-
ming the two numbers together.

-}

instance (Num a, TooMany a) => TooMany (a, a) where
         tooMany (n, n') = tooMany $ n + n'


{-

Intermission: Exercises

1. Given a datatype

data BigSmall = Big Bool
              | Small Bool
              deriving (Eq, Show)

What is the cardinality of this datatype? Hint: We already know
Bool ’s cardinality. Show your work as demonstrated earlier.

=> 4

=> Big True + Big False + Small True + Small False
=> 4


2. Given a datatype

-- needed to have Int8 in scope
import Data.Int

data NumberOrBool = Numba Int8
                  | BoolyBool Bool
                  deriving (Eq, Show)

-- Example use of Numba, parentheses due to
-- syntactic collision between (-) minus and
-- the negate function

let myNumba = Numba (-128)

What is the cardinality of NumberOrBool ? What happens if you
try to create a Numba with a numeric literal larger than 127? And
with a numeric literal smaller than (-128)?

-}

data NumberOrBool = Numba Int8
                  | BoolyBool Bool
                  deriving (Eq, Show)

{-

=> 257
=> wrap around

If you choose (-128) for a value precisely, you’ll notice you get
a spurious warning:

Prelude> let n = Numba (-128)

Literal 128 is out of the Int8 range -128..127
If you are trying to write a large negative
literal, use NegativeLiterals

Now, since -128 is a perfectly valid Int8 value you could choose to
ignore this. What happens is that (-128) desugars into (negate
128) . The compiler sees that you expect the type Int8 , but
Int8 ’s maxBound is 127. So even though you’re negating 128, it
hasn’t done that step yet and immediately whines about 128 being
larger than 127. One way to avoid the warning is the following:

Prelude> let n = (-128)
Prelude> let x = Numba n

Or you can use the NegativeLiterals extension as it recommends:

Prelude> :set -XNegativeLiterals
Prelude> let n = Numba (-128)

Note that the negative literals extension doesn’t prevent the warning
if you use negate .


Intermission: Jammin Exercises
Here we’ve started working on datatypes to keep track of Julie’s home-
made jam output, with an Int value to represent how many jars she’s
canned:

data Fruit = Peach
           | Plum
           | Apple
           | Blackberry
           deriving (Eq, Show)

data JamJars = Jam Fruit Int
             deriving (Eq, Show)

1. Let’s make a module for this. Name your module at the top of
the file:

module Jammin where

2. Rewrite JamJars with record syntax.

3. What is the cardinality of JamJars ?

4. Add Ord instances to your deriving clauses.

5. You can use the record field accessors in other functions as well.
To demonstrate this, work up some sample data that has a count
of the types and numbers of jars of jam in the rows in our pantry
(you can define more data than this if you like):

row1 = undefined

row2 = undefined

row3 = undefined

row4 = undefined

row5 = undefined

row6 = undefined

allJam = [row1, row2, row3, row4, row5, row6]

Now over that list of data, we can map the field accessor for the
Int value and see a list of the numbers for each row.

6. Write a function that will return the total number of jars of jam.

7. Write a function that will tell you which row has the most jars of
jam in it. It should return a result like this, though the fruit and
number will vary depending on how you defined your data:
*Jammin> mostRow
Jam {fruit = Apple, jars = 10}

8. Under your module name, import the module called Data.List .
It includes some standard functions called sortBy and groupBy
that will allow us to organize our list of jams. Look at their type
signatures because there are some important differences between
them.

9. You’ll want to sort the list allJams by the first field in each record.
You may (or may not) want to use the following helper function
as part of that:
compareKind (Jam k _) (Jam k' _) = compare k k'

10. Now take the sorting function and use groupBy to group the
jams by the type of fruit they are made from. You’ll later want
the ability to sum the sublists separately, so you’re looking for a
result that is a list of lists (again, the actual data in your list will
depend on how you defined it):

*Jammin> groupJam
[ [ Jam {fruit = Peach, jars = 5}
, Jam {fruit   = Peach, jars = 3} ]
, [ Jam {fruit = Plum, jars = 8}
, Jam {fruit   = Plum, jars = 4} ]
, [ Jam {fruit = Apple, jars = 10} ]
, [ Jam {fruit = Blackberry, jars = 7}
, Jam {fruit   = Blackberry, jars = 4} ] ]


Exercises

1. Given the type

data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardener = String

data Garden = Garden Gardener FlowerType
            deriving Show

What is the normal form of Garden ?

-}

data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardener = String

data Garden = Garden Gardener FlowerType
            deriving Show

data Garden' = Gardenia' Gardener
             | Daisy' Gardener
             | Rose' Gardener
             | Lilac' Gardener
             deriving Show

{-

Exercise

Write a function that generates all possible values of Programmer . Use
the provided lists of inhabitants of OperatingSystem and Program-
mingLanguage .

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux
                      , OpenBSDPlusNevermindJustBSDStill
                      , Mac
                      , Windows
                      ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = undefined

Since Programmer is a product of OperatingSystem and ProgrammingLanguage,
you can determine how many inhabitants of Programmer
you have by calculating:

length allOperatingSystems * length allLanguages

This is the essence of how product types and the number of inhabitants
relate.
If after running nub from Data.List to remove duplicate values over
your allProgrammers value, it equals the number returned by multiplying
those lengths together, you’ve probably got it figured out. Try
to be clever and make it work without manually typing out the values.

-}

data OperatingSystem = GnuPlusLinux
                     | OpenBSDPlusNevermindJustBSDStill
                     | Mac
                     | Windows
                     deriving (Bounded, Enum, Eq, Show)

data ProgrammingLanguage = Haskell
                         | Agda
                         | Idris
                         | PureScript
                         deriving (Bounded, Enum, Eq, Show)

data Programmer = Programmer
                  { os   :: OperatingSystem
                  , lang :: ProgrammingLanguage }
                  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux
                      , OpenBSDPlusNevermindJustBSDStill
                      , Mac
                      , Windows
                      ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer ops pl | ops <- allOperatingSystems,
                                      pl  <- allLanguages]

isEqLen :: Bool
isEqLen = length allOperatingSystems * length allLanguages ==
            length allProgrammers

allOperatingSystems' :: [OperatingSystem]
allOperatingSystems'                = ops : nxt (succ ops)
  where ops                         = minBound
        nxt ops' | ops' == maxBound = [ops']
                 | otherwise        = ops' : nxt (succ ops')

allLanguages' :: [ProgrammingLanguage]
allLanguages'                       = ops : nxt (succ ops)
  where ops                         = minBound
        nxt ops' | ops' == maxBound = [ops']
                 | otherwise        = ops' : nxt (succ ops')

allProgrammers' :: [Programmer]
allProgrammers' = [Programmer ops pl | ops <- allOperatingSystems',
                                       pl  <- allLanguages']

{-

Exponentiation in what order?

Consider the following function:

convert :: Quantum -> Bool
convert = undefined

According to the equality of a -> b and b a there should be 2 ^ 3 or 8
implementations of this function. Does this hold? Write it out and
prove it for yourself.

-}

data Quantum = Yes
             | No
             | Both
             deriving (Eq, Show)

-- should be 8 implementations

convert1 :: Quantum -> Bool
convert1 Yes  = True
convert1 No   = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes  = False
convert2 No   = True
convert2 Both = True

convert3 :: Quantum -> Bool
convert3 Yes  = True
convert3 No   = False
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes  = True
convert4 No   = True
convert4 Both = False

convert5 :: Quantum -> Bool
convert5 Yes  = False
convert5 No   = True
convert5 Both = False

convert6 :: Quantum -> Bool
convert6 Yes  = True
convert6 No   = False
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes  = False
convert7 No   = False
convert7 Both = False

convert8 :: Quantum -> Bool
convert8 Yes  = False
convert8 No   = False
convert8 Both = True

{-

Intermission: Exercises

Determine how many unique inhabitants each type has.

Suggestion: just do the arithmetic unless you want to verify. Writing
them out gets tedious quickly.

1.
data Quad = One
          | Two
          | Three
          | Four
          deriving (Eq, Show)

-- how many different forms can this take?

eQuad :: Either Quad Quad
eQuad = ???

-}

data Quad = One
          | Two
          | Three
          | Four
          deriving (Eq, Show)

-- 8 implementations

eQuad1 :: Either Quad Quad
eQuad1 = Left One

eQuad2 :: Either Quad Quad
eQuad2 = Left Two

eQuad3 :: Either Quad Quad
eQuad3 = Left Three

eQuad4 :: Either Quad Quad
eQuad4 = Left Four

eQuad5 :: Either Quad Quad
eQuad5 = Right One

eQuad6 :: Either Quad Quad
eQuad6 = Right Two

eQuad7 :: Either Quad Quad
eQuad7 = Right Three

eQuad8 :: Either Quad Quad
eQuad8 = Right Four

{-

2. prodQuad :: (Quad, Quad)

-}

-- 16 implementations

prodQuad1 :: (Quad, Quad)
prodQuad1 = (One, One)

prodQuad2 :: (Quad, Quad)
prodQuad2 = (One, Two)

prodQuad3 :: (Quad, Quad)
prodQuad3 = (One, Three)

prodQuad4 :: (Quad, Quad)
prodQuad4 = (One, Four)

prodQuad5 :: (Quad, Quad)
prodQuad5 = (Two, One)

prodQuad6 :: (Quad, Quad)
prodQuad6 = (Two, Two)

prodQuad7 :: (Quad, Quad)
prodQuad7 = (Two, Three)

prodQuad8 :: (Quad, Quad)
prodQuad8 = (Two, Four)

prodQuad9 :: (Quad, Quad)
prodQuad9 = (Three, One)

prodQuad10 :: (Quad, Quad)
prodQuad10 = (Three, Two)

prodQuad11 :: (Quad, Quad)
prodQuad11 = (Three, Three)

prodQuad12 :: (Quad, Quad)
prodQuad12 = (Three, Four)

prodQuad13 :: (Quad, Quad)
prodQuad13 = (Four, One)

prodQuad14 :: (Quad, Quad)
prodQuad14 = (Four, Two)

prodQuad15 :: (Quad, Quad)
prodQuad15 = (Four, Three)

prodQuad16 :: (Quad, Quad)
prodQuad16 = (Four, Four)

{-

3. funcQuad :: Quad -> Quad

-}

-- 256 implementations

funcQuad1 :: Quad -> Quad
funcQuad1 One   = One
funcQuad1 Two   = One
funcQuad1 Three = One
funcQuad1 Four  = One

funcQuad2 :: Quad -> Quad
funcQuad2 One   = Two
funcQuad2 Two   = One
funcQuad2 Three = One
funcQuad2 Four  = One

funcQuad3 :: Quad -> Quad
funcQuad3 One   = One
funcQuad3 Two   = Two
funcQuad3 Three = One
funcQuad3 Four  = One

funcQuad4 :: Quad -> Quad
funcQuad4 One   = One
funcQuad4 Two   = One
funcQuad4 Three = Two
funcQuad4 Four  = One

funcQuad5 :: Quad -> Quad
funcQuad5 One   = One
funcQuad5 Two   = One
funcQuad5 Three = One
funcQuad5 Four  = Two

-- and more... (lol)

{-

4. prodTBool :: (Bool, Bool, Bool)

-}

-- 27 implementations

prodTBool1 :: (Bool, Bool, Bool)
prodTBool1 = (True, True, True)

prodTBool2 :: (Bool, Bool, Bool)
prodTBool2 = (False, True, True)

prodTBool3 :: (Bool, Bool, Bool)
prodTBool3 = (True, False, True)

prodTBool4 :: (Bool, Bool, Bool)
prodTBool4 = (True, True, False)

-- and more...

{-

5. gTwo :: Bool -> Bool -> Bool

-}

-- 16 implementations

gTwo1 :: Bool -> Bool -> Bool
gTwo1 True True   = True
gTwo1 True False  = True
gTwo1 False True  = True
gTwo1 False False = True

-- and more...

{-

6. Hint: 5 digit number

fTwo :: Bool -> Quad -> Quad

-}

-- 60,516 implementations!!

fTwo1 :: Bool -> Quad -> Quad
fTwo1 True One    = One
fTwo1 True Two    = One
fTwo1 True Three  = One
fTwo1 True Four   = One
fTwo1 False One   = One
fTwo1 False Two   = One
fTwo1 False Three = One
fTwo1 False Four  = One

-- and mooooooore...

{-

Write map for Binary Tree

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf                = Leaf
mapTree f (Node left a right) = Node undefined undefined undefined

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
-- acceptance test for mapTree

mapOkay = if mapTree (+ 1) testTree' == mapExpected
            then print "yup okay!"
            else error "test failed!"

-}

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf                = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
-- acceptance test for mapTree

mapOkay :: IO ()
mapOkay = if mapTree (+ 1) testTree' == mapExpected
            then print "yup okay!"
            else error "test failed!"

{-

Convert Binary Trees to Lists

preorder :: BinaryTree a -> [a]
preorder = undefined

inorder :: BinaryTree a -> [a]
inorder = undefined

postorder :: BinaryTree a -> [a]
postorder = undefined

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preorder testTree == [2, 1, 3]
                 then putStrLn "Preorder fine!"
                 else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder = if inorder testTree == [1, 2, 3]
                then putStrLn "Inorder fine!"
                else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder = if postorder testTree == [1, 3, 2]
                  then putStrLn "Postorder fine!"
                  else putStrLn "postorder failed check"

main :: IO ()
main = do testPreorder
          testInorder
          testPostorder

-}

preorder :: BinaryTree a -> [a]
preorder Leaf                = []
preorder (Node left v right) = [v] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf                = []
inorder (Node left v right) = inorder left ++ [v] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf                = []
postorder (Node left v right) = postorder left ++ postorder right ++ [v]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preorder testTree == [2, 1, 3]
                 then putStrLn "Preorder fine!"
                 else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder = if inorder testTree == [1, 2, 3]
                then putStrLn "Inorder fine!"
                else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder = if postorder testTree == [1, 3, 2]
                  then putStrLn "Postorder fine!"
                  else putStrLn "postorder failed check"

main :: IO ()
main = do testPreorder
          testInorder
          testPostorder

{-

Write foldr for BinaryTree

Given the definition of BinaryTree we have provided, write a catamor-
phism for the binary trees.

-- any traversal order is fine
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b

-}

instance Foldable BinaryTree where
         foldr _ s Leaf                = s
         foldr f s (Node left v right) = v `f` foldr f (foldr f s right) left

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree = foldr

{-

Chapter Exercises

Multiple choice

1. Given the following datatype:

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday

we can say:

a) Weekday is a type with five data constructors
b) Weekday is a tree with five branches
c) Weekday is a product type
d) Weekday takes five arguments

=> a


2. and with the same datatype definition in mind, what is the type
of the following function, f?

f Friday = "Miller Time"

a) f :: [Char]
b) f :: String  -> String
c) f :: Weekday -> String
d) f :: Day     -> Beer

=> c


3. Types defined with the data keyword

a) must have at least one argument
b) must begin with a capital letter
c) must be polymorphic
d) cannot be imported from modules

=> c


4. The function g xs = xs !! (length xs - 1)

a) is recursive and may not terminate
b) delivers the head of xs
c) delivers the final element of xs
d) has the same type as xs

=> c


Ciphers

In the Lists chapter, you wrote a Caesar cipher. Now, we want to
expand on that idea by writing a Vigenère cipher. A Vigenère ci-
pher is another substitution cipher, based on a Caesar cipher, but it
uses a series of Caesar ciphers for polyalphabetic substitution. The
substitution for each letter in the plaintext is determined by a fixed
keyword.

So, for example, if you want to encode the message “meet at dawn,”
the first step is to pick a keyword that will determine which Caesar
cipher to use. We’ll use the keyword “ALLY” here. You repeat the
keyword for as many characters as there are in your original message:

MEET AT DAWN
ALLY AL LYAL

Now the number of rightward shifts to make to encode each character
is set by the character of the keyword that lines up with it. The ’A’
means a shift of 0, so the initial M will remain M. But the ’L’ for our
second character sets a rightward shift of 11, so ’E’ becomes ’P’. And
so on, so “meet at dawn” encoded with the keyword “ALLY” becomes
“MPPR AE OYWY.”

Like the Caesar cipher, you can find all kinds of resources to help you
understand the cipher and also many examples written in Haskell.
Consider using a combination of chr, ord, and mod again, possibly very
similar to what you used for writing the original Caesar cipher.

=> see Cipher2.hs


As-patterns

“As-patterns” in Haskell are a nifty way to be able to pattern match on
part of something and still refer to the entire original value. Some
examples:

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do print a
                return t

Here we pattern-matched on a tuple so we could get at the first value
for printing, but used the @ symbol to introduce a binding named t in
order to refer to the whole tuple rather than just a part.

Prelude> f (1, 2)
1
(1,2)

We can use as-patterns with pattern matching on arbitrary data con-
structors, which includes lists:

doubleUp :: [a] -> [a]
doubleUp []       = []
doubleUp xs@(x:_) = x : xs

Prelude> doubleUp []
[]

Prelude> doubleUp [1]
[1,1]

Prelude> doubleUp [1, 2]
[1,1,2]

Prelude> doubleUp [1, 2, 3]
[1,1,2,3]

Use as-patterns in implementing the following functions:

1. This should return True if (and only if) all the values in the first
list appear in the second list, though they need not be contigu-
ous.

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool

The following are examples of how this function should work:

Prelude> isSubsequenceOf "blah" "blahwoot"
True

Prelude> isSubsequenceOf "blah" "wootblah"
True

Prelude> isSubsequenceOf "blah” "wboloath"
True

Prelude> isSubsequenceOf "blah" "wootbla"
False

-}

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _                = True
isSubsequenceOf _ []                = True
isSubsequenceOf (x : xs) ys@(_ : _) = x `elem` ys && isSubsequenceOf xs ys

{-

2. Split a sentence into words, then tuple each word with the capitalized
form of each.

capitalizeWords :: String -> [(String, String)]

Prelude> capitalizeWords "hello world"

[("hello", "Hello"), ("world", "World")]

-}

capitalizeWords :: String          -> [(String, String)]
capitalizeWords = map (\s@(x : xs) -> (s, toUpper x : xs)) . words

{-

Language exercises

1. Write a function that capitalizes a word.

capitalizeWord :: String -> String
capitalizeWord = undefined

Example output.

Prelude> capitalizeWord "Titter"
"Titter"

Prelude> capitalizeWord "titter"
"Titter"

-}

capitalizeWord :: String -> String
capitalizeWord ""                   = ""
capitalizeWord (x : xs) | x == ' '  = capitalizeWord xs
                        | otherwise = toUpper x : xs

{-

2. Write a function that capitalizes sentences in a paragraph. Recognize
when a new sentence has begun by checking for periods.

Reuse the capitalizeWord function.

capitalizeParagraph :: String -> String
capitalizeParagraph = undefined

Example result you should get from your function:

Prelude> capitalizeParagraph "blah. woot ha."
"Blah. Woot ha."

-}

capitalizeParagraph :: String -> String
capitalizeParagraph = init . concatMap (capitalizeWord . (++ ". ")) . filter (/= "") . splitOn "."

{-

Phone exercise

This exercise by geophf 8 originally for 1HaskellADay. 9 Thank you for
letting us use this exercise!

Remember old-fashioned phone inputs for writing text where you
had to press a button multiple times to get different letters to come
up? You may still have to do this when you try to search for a movie
to watch using your television remote control. You’re going to write
code to translate sequences of button presses into strings and vice
versa.

So! Here is the layout of the phone:

---------------------------
| 1      | 2 ABC | 3 DEF  |
___________________________
| 4 GHI  | 5 JKL | 6 MNO  |
---------------------------
| 7 PQRS | 8 TUV | 9 WXYZ |
---------------------------
| * ^    | 0 + _ | # .,   |
---------------------------

Where star (*) gives you capitalization of the letter you’re writing to
your friends, and 0 is your space bar. To represent the digit itself, you
press that digit once more than the letters it represents. If you press
a button one more than than is required to type the digit, it wraps
around to the first letter. For example,

2     -> 'A'
22    -> 'B'
222   -> 'C'
2222  -> '2'
22222 -> 'A'

So on and so forth. We’re going to kick this around.

1. Create a data structure that captures the phone layout above.
The data structure should be able to express enough of how the
layout works that you can use it to dictate the behavior of the
functions in the following exercises.

-- fill in the rest.
data DaPhone = DaPhone

-}

data DaPhone = DaPhone
             deriving (Eq, Show)

{-

2. Convert the following conversations into the keypresses required
to express them. We’re going to suggest types and functions to
fill in order to accomplish the goal, but they’re not obligatory. If
you want to do it differently...you do you.

convo :: [String]
convo = [ "Wanna play 20 questions"
        , "Ya"
        , "U 1st haha"
        , "Lol ok. Have u ever tasted alcohol lol"
        , "Lol ya"
        , "Wow ur cool haha. Ur turn"
        , "Ok. Do u think I am pretty Lol"
        , "Lol ya"
        , "Haha thanks just making sure rofl ur turn"
        ]

-- validButtons = "1234567890*#"
type Digit      = Char

-- Valid presses: 1 and up
type Presses = Int

-- assuming the default phone definition
-- 'a'                 -> [('2', 1)]
-- 'A'                 -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps = undefined

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = undefined

-}

convo :: [String]
convo = [ "Wanna play 20 questions"
        , "Ya"
        , "U 1st haha"
        , "Lol ok. Have u ever tasted alcohol lol"
        , "Lol ya"
        , "Wow ur cool haha. Ur turn"
        , "Ok. Do u think I am pretty Lol"
        , "Lol ya"
        , "Haha thanks just making sure rofl ur turn"
        ]

-- validButtons are "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

-- assuming the default phone definition
-- 'a'                 -> [('2', 1)]
-- 'A'                 -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps _ 'a' = [('2', 1)]
reverseTaps _ 'b' = [('2', 2)]
reverseTaps _ 'c' = [('2', 3)]
reverseTaps _ 'd' = [('3', 1)]
reverseTaps _ 'e' = [('3', 2)]
reverseTaps _ 'f' = [('3', 3)]
reverseTaps _ 'g' = [('4', 1)]
reverseTaps _ 'h' = [('4', 2)]
reverseTaps _ 'i' = [('4', 3)]
reverseTaps _ 'j' = [('5', 1)]
reverseTaps _ 'k' = [('5', 2)]
reverseTaps _ 'l' = [('5', 3)]
reverseTaps _ 'm' = [('6', 1)]
reverseTaps _ 'n' = [('6', 2)]
reverseTaps _ 'o' = [('6', 3)]
reverseTaps _ 'p' = [('7', 1)]
reverseTaps _ 'q' = [('7', 2)]
reverseTaps _ 'r' = [('7', 3)]
reverseTaps _ 's' = [('7', 4)]
reverseTaps _ 't' = [('8', 1)]
reverseTaps _ 'u' = [('8', 2)]
reverseTaps _ 'v' = [('8', 3)]
reverseTaps _ 'w' = [('9', 1)]
reverseTaps _ 'x' = [('9', 2)]
reverseTaps _ 'y' = [('9', 3)]
reverseTaps _ 'z' = [('9', 4)]
reverseTaps _ 'A' = [('*', 1), ('2', 1)]
reverseTaps _ 'B' = [('*', 1), ('2', 2)]
reverseTaps _ 'C' = [('*', 1), ('2', 3)]
reverseTaps _ 'D' = [('*', 1), ('3', 1)]
reverseTaps _ 'E' = [('*', 1), ('3', 2)]
reverseTaps _ 'F' = [('*', 1), ('3', 3)]
reverseTaps _ 'G' = [('*', 1), ('4', 1)]
reverseTaps _ 'H' = [('*', 1), ('4', 2)]
reverseTaps _ 'I' = [('*', 1), ('4', 3)]
reverseTaps _ 'J' = [('*', 1), ('5', 1)]
reverseTaps _ 'K' = [('*', 1), ('5', 2)]
reverseTaps _ 'L' = [('*', 1), ('5', 3)]
reverseTaps _ 'M' = [('*', 1), ('6', 1)]
reverseTaps _ 'N' = [('*', 1), ('6', 2)]
reverseTaps _ 'O' = [('*', 1), ('6', 3)]
reverseTaps _ 'P' = [('*', 1), ('7', 1)]
reverseTaps _ 'Q' = [('*', 1), ('7', 2)]
reverseTaps _ 'R' = [('*', 1), ('7', 3)]
reverseTaps _ 'S' = [('*', 1), ('7', 4)]
reverseTaps _ 'T' = [('*', 1), ('8', 1)]
reverseTaps _ 'U' = [('*', 1), ('8', 2)]
reverseTaps _ 'V' = [('*', 1), ('8', 3)]
reverseTaps _ 'W' = [('*', 1), ('9', 1)]
reverseTaps _ 'X' = [('*', 1), ('9', 2)]
reverseTaps _ 'Y' = [('*', 1), ('9', 3)]
reverseTaps _ 'Z' = [('*', 1), ('9', 4)]
reverseTaps _ '1' = [('1', 1)]
reverseTaps _ '2' = [('2', 4)]
reverseTaps _ '3' = [('3', 4)]
reverseTaps _ '4' = [('4', 4)]
reverseTaps _ '5' = [('5', 4)]
reverseTaps _ '6' = [('6', 4)]
reverseTaps _ '7' = [('7', 5)]
reverseTaps _ '8' = [('8', 4)]
reverseTaps _ '9' = [('9', 5)]
reverseTaps _ '+' = [('0', 1)]
reverseTaps _ ' ' = [('0', 2)]
reverseTaps _ '0' = [('0', 3)]
reverseTaps _ '.' = [('#', 1)]
reverseTaps _ ',' = [('#', 2)]
reverseTaps _ '#' = [('#', 3)]
reverseTaps _ _   = error "should not happen"

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = concatMap . reverseTaps

{-

3. How many times do digits need to be pressed for each message?

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = undefined

-}

addPresses :: (Digit, Presses) -> Int -> Int
addPresses (_, p) a = p + a

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr addPresses 0

{-

4. What was the most popular letter for each message? What was its
cost? You’ll want to combine reverseTaps and fingerTaps to figure
out what it cost in taps. reverseTaps is a list because you need to
press a different button in order to get capitals.

mostPopularLetter :: String -> Char
mostPopularLetter = undefined

-}

popular :: (Eq a) => ([a] -> [a]) -> [a] -> a
popular h ss             = fst . maximumBy f . foldr ((g . h) ss) [] $ h ss
  where f (_, n) (_, n') = compare n n'
        g s i a          = (i, length (filter (== i) s)) : a

mostPopularLetter :: String -> Char
mostPopularLetter = popular $ filter isLetter

{-

5. What was the most popular letter overall? What was the most
popular word?

coolestLtr :: [String] -> Char
coolestLtr = undefined

coolestWord :: [String] -> String
coolestWord = undefined

-}

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = popular $ concatMap words

{-

Hutton’s Razor

Hutton’s Razor is a very simple expression language that expresses
integer literals and addition of values in that expression language.
The “trick” to it is that it’s recursive and the two expressions you’re
summing together could be literals or themselves further addition
operations. This sort of datatype is stereotypical of expression languages
used to motivate ideas in research papers and functional pearls.

Evaluating or folding a datatype is also in some sense what you’re
doing most of the time while programming anyway.

1. Your first task is to write the “eval” function which reduces an
expression to a final sum.

data Expr = Lit Integer
          | Add Expr Expr

eval :: Expr -> Integer
eval = error "do it to it"

Example of expected output:

Prelude> eval (Add (Lit 1) (Lit 9001))
9002

-}

data Expr = Lit Integer
          | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n)   = n
eval (Add x y) = eval x + eval y

{-

2. Write a printer for the expressions.

printExpr :: Expr -> String
printExpr = undefined

Expected output:
Prelude> printExpr (Add (Lit 1) (Lit 9001))
"1 + 9001"

Prelude> let a1 = Add (Lit 9001) (Lit 1)

Prelude> let a2 = Add a1 (Lit 20001)

Prelude> let a3 = Add (Lit 1) a2

Prelude> printExpr a3
"1 + 9001 + 1 + 20001"

-}

printExpr :: Expr -> String
printExpr (Lit n)   = show n
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y
