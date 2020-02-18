------------
-- Tuples --
------------

-- Tuples of zero values. Note that the type -- "unit" -- only has one value. 
t0 = ()

-- Tuples of two values
t2 = ("cat", "dog")

-- Tuples can contain...
t4 = ("more", "than", 1, "type")

-- Note: The parens operator can construct values instances of
-- tuple-types.

-- tuple length is part of the type of the tuple. 

-----------
-- Lists --
-----------

-- Lists must contain elements all of the same type.
stuff = ["these", "are", "strings"]

-- We use the "cons" operator or ':' to add something to the head of the list.
stuff1 = "of":stuff
stuff2 = "all":stuff1

-- Note that cons is an infix function, not a language feature!

-- We use the index operator of '!!' to select an element of a list.
word3 = stuff2 !! 3

-- Note that (!!) is an infix function, not a languge feature!

-- List length is not part of the list type.

notLegit = stuff !! 20

-- Some functions on lists
--
-- head
-- tail
-- init
-- last

-- Some more functions on lists
--
-- elem
-- zip

-- Some higher order functions on lists. 
-- 
-- map
-- foldl
-- filter
-- takeWhile

-- Comprehensions and generators
list0 = [0,2..10]

list1 = [x**2 | x <- list0]

list2 = [if x `mod` 2 == 0 then "even" else "odd" | x <- [1..10]]

digits = [0..9]
list3 = [10*x + y | x <- digits, y <- digits]

list4 = [10*x + y | x <- digits, y <- digits, x+y == 7]

---------------
-- Functions -- 
---------------

-- We can use normal functions as infix functions with backticks.
plus x y = x + y
g x y = x `plus` y

-- Haskell offers quite a wide latitude to create non-alphanumberic
-- infix functions. We use parens around non-alaphanumeric
-- characters. 
(<!) x y = x + y
(#>) x y = x - y
(@+*) x y = x * y
(&%--) x y = x / y

totallyLegit = 6 <! 5 #> 4 @+* 3 &%-- 2

--- Note: Just because you can do something ...



-- Pattern matching on a tuple to create a new reversed tuple. Here x
-- is bound to the first argment and y is bound to the second.
f0 (x, y) = (y, x)

-- Often we use _ to indicate we don't care about the variable
f1 (x, _) = x

-- We can also ues an @ to bind the entire tuple to t.
f2 t@(x, y) = (t, x, y)

-- We can use pattern matching and multiple equations to good effect. 
f3 [] = "Empty list"
f3 [x] = "List with one element"
f3 xs = "List with more than one element"

-- Question: How did the type inferencer assess the last equation for f3?

-- We can, and often do, pattern match on the cons operator. Parens
-- are required for precedence. This allows us to extract the first
-- element of the list.
f4 [] = "Empty List"
f4 (x:xs) = "Not empty list. It has " ++ (show x) ++ " and also " ++ (show xs)

-- Comments:
-- 1. we can merge lists with ++,
-- 2. strings, so far, are lists. We'll study better strings later
-- 3. show (often) provides a string representation for the argument.

-- Let's write our own map, we'll call it map'

map' f [] = []
map' f (x:xs) = f x:map' f xs

-- Question: Why is the first line of map' necessary?


-- Guards. We use a pipe '|' followed by a test to determine which
-- branch we pursue.
safeDivision n d
  | d /= 0    = n/d
  | otherwise = 127.0


abs' x
  | x > 0 = x
  | otherwise = -x


isSqrt s x
  | abs' (s*s - x) < 0.01 = "Yes!"
  | abs' (s*s - x) < 0.1  = "I guess..."
  | otherwise             = "Umm... we've got standards."


-- Two useful tools for repeated expressions

someFunc x =
  let ax = abs' x
  in ax/(1 + ax)


someFunc' x =
  ax/(1 + ax)
  where
    ax = abs' x


-- Making factorial tail recursive.

fact0 1 = 1
fact0 n = n * fact0 (n-1)


fact1 1 = 1
fact1 n = go n 1
  where
    go 1 a = a
    go n a = go (n-1) (n*a)

