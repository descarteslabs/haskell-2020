-- Debugging
-- Searches over lists
-- Performance stuff

import Debug.Trace (trace)
import Control.Monad (guard)

add a b = trace (">>adding " ++ (show a) ++ " and " ++ (show b) ++ "<<") $ a + b

main1 = do
  xStr <- getLine
  yStr <- getLine
  let x = read xStr :: Int
  let y = read yStr :: Int
  let s = add x 12
  putStrLn $ show s


die = [1..6]

f = do
  x <- die
  y <- die
  z <- die
  guard $ x + y + z == 9
  [(x, y, z)]

g =
  die >>=
  (\x -> die >>=
    (\y -> die >>=
      (\z -> [(x,y,z)] >>
        (guard $ x+y+z == 9) >>
        [(x,y,z)]
      )
    )
  )

--   send 
-- + more
-- ------
--  money

digits = [0..9] :: [Int]

remove :: [Int] -> [Int] -> [Int]
remove [] bs = bs
remove (a:as) bs = filter (/= a) $ remove as bs

digitsToNum :: [Int] -> Int
digitsToNum ds = (go 0 ds)
  where
    go :: Int -> [Int] -> Int
    go acc (d:ds) = go (10*acc + d) ds
    go acc [] = acc

solution = do
  s <- remove [0] digits
  m <- remove [s, 0] digits

  let minSend = s*1000
  let maxSend = s*1000 + 999

  let minMore = m*1000
  let maxMore = m*1000 + 999

  let minMoney = m * 10000
  let maxMoney = m * 10000 + 9999

  guard $ minSend + minMore <= maxMoney
  guard $ maxSend + maxMore >= minMoney

  e <- remove [s, m] digits
  n <- remove [s, m, e] digits
  d <- remove [s, m, e, n] digits
  let send = digitsToNum [s,e,n,d]
  o <- remove [s, e, n, d, m] digits
  r <- remove [s, e, n, d, m, o] digits
  let more = digitsToNum [m, o, r, e]
  let sendPlusMore = send + more
  y <- remove [s, e, n, d, m, o, r] digits
  let money = digitsToNum [m, o, n, e, y]
  guard $ send + more == money
  [(send, more, money)]

main = putStrLn $ show solution
