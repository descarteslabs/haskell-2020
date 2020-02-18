




-- This is a comment.

-- Here is a function of zero arguemnts. It's a constnat.
f0 = 2

-- Here is a function of one argument
f1 :: Int -> Int
f1 x = x + 2

-- Here is a function of two arguments
f2 :: Int -> Int -> Int
f2 x y = x + 2*y

-- Here is a function in two equations
f3 1 = 0
f3 x = 1

h x = 2*x

-- Here are functions with an if-statement and recursion
fib n =
  if n < 3
  then 1
  else fib (n-1) + fib (n-2)

fact n =
  if n < 2
  then 1
  else n * fact (n-1)

-- This is a list
someNumbers = [1..10]

-- This is another list
allNumbers = [1..]

