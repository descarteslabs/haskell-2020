import Debug.Trace (trace)
import Control.Monad (guard)

-- Debugging: not canonical, as Haskell prgrammers don't make errors.                   
--                                                                                      
-- ... but for the rest of us hooligans who are just taking GHC out                     
-- for a joyride ...                                                                    

oops = do
  xStr <- getLine
  let x = read xStr :: Int
  yStr <- getLine
  let y = read yStr :: Int
  -- Note: this line seems to give the wrong answer for the sum of x                    
  -- and y except when the second argument is 12. I can't figure this                   
  -- out.                                                                               
  let s = add x 12
  putStrLn $ show x ++ " plus " ++ show y ++ " equals " ++ show s

add x y =
  trace (">>Adding " ++ show x ++ " and " ++ show y ++ "<<") $ x + y

-- More experimenting with Monads                                                       

-- A basic search of lists                                                              
die = [1..6]

f = do
  x <- die
  y <- die
  guard $ x + y == 9
  [(x,y)]

g =
  die >>= (\x ->
   die >>= (\y ->
    (guard $ x + y == 9) >>= (
     \_ -> [(x,y)]
     )
   )
  )

-- A more complex search of lists                                                       

--    send                                                                              
-- +  more                                                                              
-- -------                                                                              
--   money                                                                              

remove :: [Int] -> [Int] -> [Int]
remove [] bs = bs
remove (a:as) bs =
  remove as $ filter (/= a) bs

digitsToNum :: [Int] -> Int
digitsToNum ds = (go 0 ds)
  where
    go :: Int -> [Int] -> Int
    go acc (d:ds) = go (10*acc + d) ds
    go acc [] = acc



solve :: [(Int, Int, Int)]
solve = do
  let digits = [0..9] :: [Int]
  s <- remove [0] digits
  e <- remove [s] digits
  n <- remove [s, e] digits
  d <- remove [s, e, n] digits
  let send = digitsToNum [s, e, n, d]
  m <- remove [0, s, e, n, d] digits
  o <- remove [s, e, n, d, m] digits
  r <- remove [s, e, n, d, m, o] digits
  let more = digitsToNum [m, o, r, e]
  y <- remove [s, e, n, d, m, o, r] digits
  let money = digitsToNum [m, o, n, e, y]
  guard $ send + more == money
  return (send, more, money)

solve1 :: [(Int, Int, Int)]
solve1 = do
  let digits = [0..9] :: [Int]
  s <- remove [0] digits
  m <- remove [0, s] digits

  let minSend = s*1000
  let minMore = m*1000
  let maxMoney = m*10000 + 9999
  guard $ minSend + minMore < maxMoney

  let maxSend = minSend + 999
  let maxMore = minMore + 999
  let minMoney = maxMoney - 9999
  guard $ maxSend + maxMore > minMoney

  e <- remove [s, m] digits
  n <- remove [m, s, e] digits
  d <- remove [m, s, e, n] digits
  let send = digitsToNum [s, e, n, d]
  o <- remove [s, e, n, d, m] digits
  r <- remove [s, e, n, d, m, o] digits
  let more = digitsToNum [m, o, r, e]
  y <- remove [s, e, n, d, m, o, r] digits
  let money = digitsToNum [m, o, n, e, y]
  guard $ send + more == money
  return (send, more, money)

main = do
  let (send, more, money) = solve1 !! 0
  putStrLn $ "  " ++ show send
  putStrLn $ "+ " ++ show more
  putStrLn  "------"
  putStrLn $ " " ++ show money
