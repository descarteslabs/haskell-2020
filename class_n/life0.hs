import qualified System.Random as R
import qualified Control.Concurrent as C

main :: IO ()
main = do
  boardSize <- getBoardSize
  numTurns <- getNumTurns
  board <- genBoard boardSize
  printBoard board
  play numTurns board


getBoardSize :: IO (Int, Int)
getBoardSize = do
  putStrLn "Board size?"
  boardSizeStr <- getLine
  let boardSize = parseBoardSize boardSizeStr
  putStrLn $ "Board size: " ++ (show $ boardSize)
  return boardSize


parseBoardSize s = (read s2a :: Int, read s4 :: Int)
  where
    s1 = dropWhile isn'tDigit s
    s2a = takeWhile isDigit s1
    s2b = dropWhile isDigit s1
    s3 = dropWhile isn'tDigit s2b
    s4 = takeWhile isDigit s3
    isDigit c = c `elem` digits
    isn'tDigit = not . isDigit
    digits = "0123456789"


getNumTurns :: IO Int
getNumTurns = do
  putStrLn "Num turns?"
  numTurnsStr <- getLine
  let numTurns = read numTurnsStr :: Int
  putStrLn $ "Num turns: " ++ (show numTurns)
  return numTurns


genBoard :: (Int, Int) -> IO [[Int]]
genBoard (rows, cols) = do 
  let numSpaces = rows * cols
  g <- R.getStdGen
  let vals = take numSpaces (R.randomRs (0, 1) g)
  return $ reshape rows cols vals


reshape :: Int -> Int -> [a] -> [[a]]
reshape rows cols vals =
  map f [0..(rows-1)]
  where
    f r = map (g r) [0..(cols-1)]
    g r c = vals !! (r*cols + c)


printBoard :: [[Int]] -> IO ()
printBoard board =
  putStrLn $ showBoard board


showBoard :: [[Int]] -> [Char]
showBoard board =
  unlines $ t ++ (map g board) ++ t
  where
    f 1 = 'H'
    f 0 = ' '
    g row = "|" ++ (map f row) ++ "|"
    cols = length $ board !! 0
    t = [map (\_ -> '-') [0..(cols+1)]]


play :: Int -> [[Int]] -> IO ()
play i board =
  if 0 == i
  then return ()
  else do
    C.threadDelay 500000
    let newBoard = updateBoard board
    printBoard newBoard
    play (i-1) newBoard


updateBoard :: [[Int]] -> [[Int]]
updateBoard board = 
  reshape rows cols $ map (\(r, c) -> updateRC r c board) boardIdxs
  where
    rows = length board
    cols = length $ board !! 0
    boardIdxs =
      [(r, c) |
       r <- [0..(rows-1)],
       c <- [0..(cols-1)]]


updateRC :: Int -> Int -> [[Int]] -> Int
updateRC row col board =
  case s of 2 -> (board !! row !! col)
            3 -> 1
            otherwise -> 0
  where
    rows = length board
    cols = length $ board !! 0
    r1 = flip mod rows
    c1 = flip mod cols
    nbhd =
      [(r, c) |
        r <- map r1 [(row-1)..(row+1)],
        c <- map c1 [(col-1)..(col+1)]]
    s0 = sum $ map (\(r, c) -> board !! r !! c) nbhd
    s = s0 - (board !! row !! col)
