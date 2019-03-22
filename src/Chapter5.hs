{-# LANGUAGE NoImplicitPrelude #-}

module Chapter5 where

import Data.Bool     (Bool)
import Data.Char     (Char)
import Data.Eq       (Eq)
import Data.Function (($))
import Data.Int      (Int)
import Data.List     (length, take, (++))
import Data.Ord      (Ord, (<), (>))
import Data.String   (String)
import Data.Tuple    (fst)
import GHC.Err       (undefined)
import GHC.Integer   (Integer)
import GHC.Num       (Num, (*), (+))
import GHC.Real      (Fractional, (/), (^))
import System.IO     (IO, print)

{-

Intermission: Exercises

Below you’ll find a list of several standard functions we’ve talked about
previously. Under that is a list of their type signatures. Match the
function to its type signature. Try to do it without peeking at the type
signatures (either in the text or in GHCi) and then check your work.
You may find it easier to start from the types and work out what you
think a function of that type would do.

1. Functions:

a) not => c

b) length => d

c) concat => b

d) head => a

e) (<) => e


2. Type signatures:

a) _ :: [a] -> a

b) _ :: [[a]] -> [a]

c) _ :: Bool -> Bool

d) _ :: [a] -> Int

e) _ :: Ord a => a -> a -> Bool


Intermission: Exercises

Given a function and its type, tell us what type results from applying
some or all of the arguments.

1. If the type of f is a -> a -> a -> a , and the type of x is Char
then the type of f x is
=> a

a) Char -> Char -> Char

b) x -> x -> x -> x

c) a -> a -> a

d) a -> a -> a -> Char

-}

f :: a -> a -> a -> a
f = undefined

{-

2. If the type of g is a -> b -> c -> b , then the type of
g 0 'c' "woot" is
=> d

a) String

b) Char -> String

c) Int

d) Char

-}

g :: a -> b -> c -> b
g = undefined

{-

3. If the type of h is (Num a, Num b) => a -> b -> b , then the
type of
h 1.0 2 is
=> d

a) Double

b) Integer

c) Integral b => b

d) Num b => b

-}

h :: (Num a, Num b) => a -> b -> b
h = undefined

{-

4. If the type of h is (Num a, Num b) => a -> b -> b , then the
type of
h 1 (5.5 :: Double) is
=> c

a) Integer

b) Fractional b => b

c) Double

d) Num b => b

-}

h' :: (Num a, Num b) => a -> b -> b
h' = undefined

{-

5. If the type of jackal is (Ord a, Eq b) => a -> b -> a , then
the type of
jackal "keyboard" "has the word jackal in it"
=> a

a) [Char]

b) Eq b => b

c) b -> [Char]

d) b

e) Eq b => b -> [Char]

-}

jackal :: (Ord a, Eq b) => a -> b -> a
jackal = undefined

{-

6. If the type of jackal is (Ord a, Eq b) => a -> b -> a , then
the type of
jackal "keyboard"
=> e

a) b

b) Eq b => b

c) [Char]

d) b -> [Char]

e) Eq b => b -> [Char]

-}

jackal' :: (Ord a, Eq b) => a -> b -> a
jackal' = undefined

{-

7. If the type of kessel is (Ord a, Num b) => a -> b -> a , then
the type of
kessel 1 2 is
=> d

a) Integer

b) Int

c) a

d) (Num a, Ord a) => a

e) Ord a => a

f) Num a => a

-}

kessel :: (Ord a, Eq b) => a -> b -> a
kessel = undefined

{-

8. If the type of kessel is (Ord a, Num b) => a -> b -> a , then
the type of
kessel 1 (2 :: Integer) is
=> a

a) (Num a, Ord a) => a

b) Int

c) a

d) Num a => a

e) Ord a => a

f) Integer

-}

kessel' :: (Ord a, Num b) => a -> b -> a
kessel' = undefined

{-

9. If the type of kessel is (Ord a, Num b) => a -> b -> a , then
the type of
kessel (1 :: Integer) 2 is
=> c

a) Num a => a

b) Ord a => a

c) Integer

d) (Num a, Ord a) => a

e) a

-}

kessel'' :: (Ord a, Num b) => a -> b -> a
kessel'' = undefined

{-

Intermission: Exercises

All you can really do with a parametrically polymorphic value is pass
or not pass it to some other expression. Prove that to yourself with
these small demonstrations.

1. Given the type a -> a , which is the type for id - attempt to
make it do something other than returning the same value. This
is impossible, but you should try it anyway.

=> impossible

2. We can get a more comfortable appreciation of parametricity by
looking at a  -> a -> a . This hypothetical function a -> a ->
a has two–and only two–implementations. Write both possible
versions of a -> a -> a , then try to violate the constraints we’ve
described.

-}

hypo :: a -> a -> a
hypo xx _ = xx

-- hypo _ y = y

{-

3. Implement a -> b -> b . How many implementations can it
have? Does the behavior change when the types of a and b
change?

-}

hypo' :: a -> b -> b
hypo' _ yy = yy

{-

=> 1
=> no


Intermission: Exercises

Look at these pairs of functions. One function is unapplied, so the
compiler will infer maximally polymorphic type. The second func-
tion has been applied to a value, so the inferred type signature may
have become concrete, or at least less polymorphic. Figure out how
the type would change and why, make a note of what you think the
new inferred type would be and then check your work in GHCi.

1.
-- Type signature of general function
(++) :: [a] -> [a] -> [a]
-- How might that change when we apply
-- it to the following value?
myConcat x = x ++ " yo"

-}

myConcat :: String -> String
myConcat xx = xx ++ " yo"

{-

2.
-- General function
(*) :: Num a => a -> a -> a
-- Applied to a value
myMult x = (x / 3) * 5

-}

myMult :: Fractional a => a -> a
myMult xx = (xx / 3) * 5

{-

3.
take :: Int -> [a] -> [a]
myTake x = take x "hey you"

-}

myTake :: Int -> String
myTake xx = take xx "hey you"

{-

4.
(>) :: Ord a => a -> a -> Bool
myCom x = x > (length [1..10])

-}

myCom :: Int -> Bool
myCom xx = xx > length [1..10]

{-

5.
(<) :: Ord a => a -> a -> Bool
myAlph x = x < 'z'

-}

myAlph :: Char -> Bool
myAlph ex = ex < 'z'

{-

Chapter Exercises

Multiple choice

1. A value of type [a] is
a) a list of alphabetic characters
b) a list of lists
c) a list whose elements are all of some type a
d) a list whose elements are all of different types
=> c

2. A function of type [[a]] -> [a] could
a) take a list of strings as an argument
b) transform a character into a string
c) transform a string in to a list of strings
d) take two arguments
=> a

3. A function of type [a] -> Int -> a
a) takes one argument
b) returns one element of type a from a list
c) must return an Int value
d) is completely fictional
=> b

4. A function of type (a, b) -> a
a) takes a list argument and returns a Char value
b) has zero arguments
c) takes a tuple argument and returns the first value
d) requires that a and b be of different types
=> c


Determine the type

For the following functions, determine the type of the specified value.
Note: you can type them into a file and load the contents of the file in
GHCi. You can then query the types after you’ve loaded them.

1. All function applications return a value. Determine the value
returned by these function applications and the type of that
value.

a) (* 9) 6
=> 54
=> Int

b) head [(0,"doge"),(1,"kitteh")]
=> (0, "doge")
=> (Int, String)

c) head [(0 :: Integer ,"doge"),(1,"kitteh")]
=> (0       :: Integer, "doge")
=> (Integer, String)

d) if False then True else False
=> False
=> Bool

e) length [1, 2, 3, 4, 5]
=> 5
=> Int

f) (length [1, 2, 3, 4]) > (length "TACOCAT")
=> False
=> Bool

2. Given
x = 5
y = x + 5
w = y * 10
What is the type of w?
=> Num a => a

3. Given
x   = 5
y   = x + 5
z y = y * 10
What is the type of z?
=> Num a -> a -> a

4. Given
x = 5
y = x + 5
f = 4 / y
What is the type of f?
=> Fractional a => a

5. Given
x = "Julie"
y = " <3 "
z = "Haskell"
f = x ++ y ++ z
What is the type of f?
=> String


Does it compile?
For each set of expressions, figure out which expression, if any, causes
the compiler to squawk at you (n.b. we do not mean literal squawking)
and why. Fix it if you can.

1.

bigNum = (^) 5 $ 10

wahoo = bigNum $ 10
=> wahoo wont compile

-}

bigNum :: Integer
bigNum = 5 ^ 10

wahoo :: Integer
wahoo = bigNum * 10

{-

2.

x = print

y = print "woohoo!"

z = x "hello world"
=> will compile
-}

x :: String -> IO ()
x = print

y :: IO ()
y = print "woohoo!"

z :: IO ()
z = x "hello world"

{-

3.
a = (+)
b = 5
c = b 10
d = c 200
=> won't compile
-}

a :: Num a => a -> a -> a
a = (+)

b :: Int
b = 5

c :: Int -> Int
c = a 10

d :: Int
d = c 200

{-

4.
a = 12 + b
b = 10000 * c

=> will not compile

-}

a' :: Int
a' = 12 + b'

b' :: Int
b' = 10000 * c'

c' :: Int
c' = 1

{-

Type variable or specific type constructor?

1. You will be shown a type declaration, and you should categorize
each type. The choices are a fully polymorphic type variable,
constrained polymorphic type variable, or concrete type constructor.
f :: Num a => a -> b -> Int -> Int
--           [0]  [1]   [2]    [3]
Here, the answer would be: constrained polymorphic (Num),
fully polymorphic, concrete, and concrete.

2. Categorize each component of the type signature as described
in the previous example.
f :: zed -> Zed -> Blah
=> fully polymorphic
=> concrete
=> concrete

3. Categorize each component of the type signature
f :: Enum b => a -> b -> C
=> fully polymorphic
=> constrained polymorphic
=> concrete

4. Categorize each component of the type signature
f :: f -> g -> C
=> fully polymorphic
=> fully polymorphic
=> concrete


Write a type signature

For the following expressions, please add a type signature. You should
be able to rely on GHCi type inference to check your work, although
you might not have precisely the same answer as GHCi gives (due to
polymorphism, etc).

1. While we haven’t fully explained this syntax yet, you’ve seen it
in Chapter 2 and as a solution to an exercise in Chapter 4. This
syntax is a way of destructuring a single element of a list.
functionH ::
functionH (x:_) = x

-}

functionH :: [a] -> a
functionH (headz : _) = headz
functionH []          = undefined

{-

2. functionC ::
functionC x y = if (x > y) then True else False

-}

functionC :: (Ord a) => a -> a -> Bool
functionC p1 p2 = p1 > p2

{-

3. functionS ::
functionS (x, y) = y
-}

functionS :: (a, b) -> b
functionS (_, yy) = yy

{-

Given a type, write the function

You will be shown a type and a function that needs to be written. Use
the information the type provides to determine what the function
should do. We’ll also tell you how many ways there are to write the
function. (Syntactically different but semantically equivalent imple-
mentations are not counted as being different).

1. There is only one implementation that typechecks.
i :: a -> a
i = undefined

-}

i :: a -> a
i x' = x'

{-

2. There is only one version that works.
c :: a -> b -> a
c = undefined

-}

c'' :: a -> b -> a
c'' x' _ = x'

{-

3. Given alpha equivalence are c” and c (see above) the same thing?
c'' :: b -> a -> b
c'' = ?

-}

c''' :: b -> a -> b
c''' x' _ = x'
-- same as above

{-

4. Only one version that works.
c' :: a -> b -> b
c' = undefined

-}

c'''' :: a -> b -> b
c'''' _ y' = y'

{-

5. There are multiple possibilities, at least two of which you’ve seen
in previous chapters.
r :: [a] -> [a]
r = undefined

-}

r :: [a] -> [a]
r xs = xs

r' :: [a] -> [a]
r' []          = []
r' (hedz : xs) = r xs ++ [hedz]

{-

6. Only one version that will typecheck.
co :: (b -> c) -> (a -> b) -> (a -> c)
co = undefined

-}

co :: (b -> c) -> (a -> b) -> (a -> c)
co f' g' arg = f' ( g' arg)

{-

7. One version will typecheck.
a :: (a -> c) -> a -> a
a = undefined

-}

a'''' :: (a -> c) -> a -> a
a'''' _ arg = arg

{-

8. One version will typecheck.
a' :: (a -> b) -> a -> b
a' = undefined

-}

a''''' :: (a -> b) -> a -> b
a''''' func = func

{-

Fix it

Won’t someone take pity on this poor broken code and fix it up? Be
sure to check carefully for things like capitalization, parentheses, and
indentation.

1.
module sing where
fstString :: [Char] ++ [Char]
fstString x = x ++ " in the rain"
sndString :: [Char] -> Char
sndString x = x ++ " over the rainbow"
sing        = if (x > y) then fstString x or sndString y
where x     = "Singin"
x           = "Somewhere"

=> See Sing.hs

2. Now that it’s fixed, make a minor change and make it sing the
other song. If you’re lucky, you’ll end up with both songs stuck
in your head!

=> change > to <

3.
-- arith3broken.hs
module Arith3Broken where
main :: IO ()
Main       = do
print 1 + 2
putStrLn 10
print (negate -1)
print ((+) 0 blah)
where blah = negate 1

=> See Arith3Broken.hs


Type-Kwon-Do

The name is courtesy Phillip Wright, thank you for the idea!
The focus here is on manipulating terms in order to get the types
to fit. This sort of exercise is something you’ll encounter in writing
real Haskell code, so the practice will make it easier to deal with when
you get there. Practicing this will make you better at writing ordinary
code as well.

We provide the types and bottomed out (declared as “undefined”)
terms. Bottom and undefined will be explained in more detail later.
The contents of the terms are irrelevant here. You’ll use only the
declarations provided and what the Prelude provides by default unless
otherwise specified. Your goal is to make the ??? ’d declaration pass
the typechecker by modifying it alone.

Here’s a worked example for how we present these exercises and how
you are expected to solve them. Given the following:

data Woot
data Blah

f :: Woot -> Blah
f = undefined

g :: (Blah, Woot) -> (Blah, Blah)
g = ???

Here it’s g that you’re supposed to implement, however you can’t eval-
uate anything. You’re to only use type-checking and type-inference
to validate your answers. Also note that we’re using a trick for defining
datatypes which can be named in a type signature, but have no values.
Here’s an example of a valid solution:

g :: (Blah, Woot) -> (Blah, Blah)
g (b, w) = (b, f w)

The idea is to only fill in what we’ve marked with ??? .
Not all terms will always be used in the intended solution for a problem.


1.
f :: Int    -> String
f = undefined
g :: String -> Char
g = undefined
h :: Int    -> Char
h = ???

-}

fx :: Int -> String
fx = undefined

gx :: String -> Char
gx = undefined

hx :: Int -> Char
hx arg = gx $ fx arg

{-

2.
data A
data B
data C
q :: A -> B
q = undefined
w :: B -> C
w = undefined
e :: A -> C
e = ???

-}

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e arg = w $ q arg

{-

3.
data X
data Y
data Z
xz    :: X      -> Z
xz    = undefined
yz    :: Y      -> Z
yz    = undefined
xform :: (X, Y) -> (Z, Z)
xform = ???

-}

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (ex, wai) = (xz ex, yz wai)

{-

4.
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge = ???

-}

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge fe ge ex = fst $ ge $ fe ex
