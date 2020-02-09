prime n = seive [2..] !! n
  where
    seive (x:xs) = x:[y | y <- (seive xs), y `mod` x /= 0]

fib n = fibs !! n
  where
    fibs = 1:1:zipWith (+) (drop 1 fibs) fibs

fact n = foldl (*) 1 [1..n]

sort [] = []
sort [x] = [x]
sort (x:xs) = [y | y <- sort lo] ++ [x] ++ [y | y <- sort hi]
  where
    lo = filter (< x) xs
    hi = filter (> x) xs
