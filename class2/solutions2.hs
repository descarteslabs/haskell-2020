-- Write a function that computes the length of a list.
-- Recursion and pattern matching will be helpful.

listLength [] = 0
listLength (x:xs) = 1 + listLength xs

-- Write a function that computes the length of a list that is
-- tail-recursive (We'll get to this this week)

tailRecursiveListLength = undefined


-- Write thd (third) for 3-tuples. Hint: it’s a one-liner with pattern
-- matching!

thd (_, _, x) = x

-- The function foldl is akin to a generalized summation. Write a
-- function that sums a list of numbers using foldl.

sum' xs = foldl (\x y -> x + y) 0 xs
-- Note that we've introduced Haskell's lambda function syntax here
--
-- \x y -> x + y
--
-- is an example of
--
-- \<arguments separated by spaces> -> <expression>

-- Use foldl to compute the total number of characters in a list of
-- strings.

totalChars :: [[Char]] -> Int
totalChars xs = sum' ls
  where ls = map listLength xs
