{-# LANGUAGE NoImplicitPrelude #-}

module Chapter2 where

import GHC.Num  (Num, (*), (+))
import GHC.Real ((^))

{-

Hello, Haskell!

Intermission: Exercises

1. Given the following lines of code as they might appear in a
source file, how would you change them to use them directly in
the REPL?
half x        = x / 2
=> let half x = x / 2

square x        = x * x
=> let square x = x * x

2. Write one function that can accept one argument and work for
all the following expressions. Be sure to name the function.
3.14 * (5 * 5)
3.14 * (10 * 10)
3.14 * (2 * 2)
3.14 * (4 * 4)

pisqr n      = pi * n * n
--OR
let pisqr' n = pi * n * n

Intermission: Exercises

Below are some pairs of functions that are alike except for parenthe-
sization. Read them carefully and decide if the parentheses change
the results of the function. Check your work in GHCi.

1. a) 8 + 7 * 9   => 71
   b) (8 + 7) * 9 => 135

2, a) perimeter x y = (x * 2) + (y * 2) => 6
   b) perimeter x y = x * 2 + y * 2     => 6

3. a) f x = x / 2 + 9
   b) f x = x / (2 + 9)

Intermission: Exercises

The following code samples are broken and won’t compile. The first
two are as you might enter into the REPL; the third is from a source
file. Find the mistakes and fix them so that they will.

1. let area x = 3. 14 * (x * x)

=> let area x = 3.14 * (x * x)

2. let double x = b * 2

=> let double b = b * 2

3. x  = 7
    y = 10
   f  = x + y

=> x = 7
   y = 10
   f = x + y

Intermission: Exercises

Now for some exercises. First, determine in your head what the
following expressions will return, then validate in the REPL:

1. let x = 5 in x
=> 5

2. let x = 5 in x * x
=> 25

3. let x = 5; y = 6 in x * y
=> 30

4. let x = 3; y = 1000 in x + 3
=> 1003

More exercises!

Rewrite the following let expressions into declarations with where
clauses:

1. let x = 3; y = 1000 in x * 3 + y

=> f         = x * 3 + y
     where x = 3
           y = 1000

2. let y = 10; x = 10 * 5 + y in x * 5

=> f         = x * 5
     where y = 10
           x = 10 * 5 * y

3. let x = 7; y = negate x; z = y * 10 in z / x + y

=> f         = z / x + y
     where x = 7
           y = negate x
           z = y * 10

Chapter Exercises

The goal for all the following exercises is just to get you playing with
code and forming hypotheses about what it should do. Read the code
carefully, using what we’ve learned so far. Generate a hypothesis
about what you think the code will do. Play with it in the REPL and
find out where you were right or wrong.

Attempt the above on the following expressions.

1. 2 + 2 * 3 - 1
=> 2 + (2 * 3) - 1

2. (^) 10 $ 1 + 1
=> (^) 10 (1 + 1)

3. 2 ^ 2 * 4 ^ 5 + 1
=> ((2 ^ 2) * (4 ^ 5)) + 1

Equivalent expressions

Which of the following pairs of expressions will return the same result
when evaluated? Try to reason them out in your head by reading the
code and then enter them into the REPL to check your work:

1.
1 + 1
2
=> same

2.
10 ^ 2
10 + 9 * 10
=> same

3.
400 - 37
(-) 37 400
=> not same

4.
100 `div` 3
100 / 3
=> not same

5.
2 * 5 + 18
2 * (5 + 18)
=> not same

More fun with functions

Here is a bit of code as it might be entered into a source file. Remember
that when you write code in a source file, the order is unimportant, but
when writing code directly into the REPL the order does matter. Given
that, look at this code and rewrite it such that it could be evaluated in
the REPL (remember: you’ll need let when entering it directly into
the REPL). Be sure to enter your code into the REPL to make sure it
evaluates correctly.

z     = 7
x     = y ^ 2
waxOn = x * 5
y     = z + 8

=> let z     = 7
   let y     = z + 8
   let x     = y ^ 2
   let waxOn = x * 5

1. Now you have a value called waxOn in your REPL. What do you
think will happen if you enter:
10 + waxOn => 1135
-- or
(+10) waxOn => 1135
-- or
(-) 15 waxOn => -1110
-- or
(-) waxOn 15 => 1110

2. Earlier we looked at a function called triple . While your REPL
has waxOn in session, re-enter the triple function at the prompt:
let triple x    = x * 3
=> let triple x = x * 3

3. Now, what will happen if we enter this at our GHCi prompt. Try
to reason out what you think will happen first, considering what
role waxOn is playing in this function call. Then enter it, see what
does happen, and check your understanding:
triple waxOn
=> 3375

4. Rewrite waxOn as a function with a where clause in your source
file. Load it into your REPL and make sure it still works as expected!
=> look below

5. Now to the same source file where you have waxOn , add the
triple function. Remember: You don’t need let and the func-
tion name should be at the left margin (that is, not nested as one
of the waxOn expressions). Make sure it works by loading it into
your REPL and then entering triple waxOn again at the REPL
prompt. You should have the same answer as you did above.
=> 3375

6. Now, without changing what you’ve done so far in that file, add
a new function called waxOff that looks like this:
waxOff x = triple x
=> look below

7. Load the source file into your REPL and enter waxOff waxOn at the prompt.
You now have a function, waxOff that can be applied to a variety
of arguments — not just waxOn but any (numeric) value you want
to put in for x. Play with that a bit. What is the result of waxOff
10 or waxOff (-50) ? Try modifying your waxOff function to do
something new — perhaps you want to first triple the x value and
then square it or divide it by 10. Just spend some time getting
comfortable with modifying the source file code, reloading it,
and checking your modification in the REPL.
waxOff 10
=> 30
waxOff (-50)
=> -150

-}

z :: Num a => a
z = 7

y :: Num a => a
y = z + 8

x :: Num a => a
x = y ^ 2

waxOn :: Num a => a
waxOn = x * 5

triple :: Num a => a -> a
triple x' = x' * 3

waxOn' :: Num a => a
waxOn'     = x' * 5
  where z' = 7
        y' = z' + 8
        x' = y' ^ 2

waxOff :: Num a => a -> a
waxOff = triple
