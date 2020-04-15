import qualified Control.Concurrent as C

data Args = Args {
    rows :: Int
  , cols :: Int
  , numIters :: Int
  } deriving (Show, Read)


main :: IO ()
main = do
  argStr <- readLn :: IO String
  let args = read argStr :: Args 
  putStrLn $ show args
  let board = makeBoard (rows args) (cols args)
  putStrLn $ showBoard board
  playLife (numIters args) board 


playLife :: Int -> [[Int]] -> IO ()
playLife iters board =
  if 0 == iters
  then return ()
  else do
    let newBoard = updateBoard board
    putStrLn $ showBoard newBoard
    C.threadDelay 500000
    playLife (iters - 1) newBoard
    

makeBoard :: Int -> Int -> [[Int]]
makeBoard rows cols =
  (take n $ repeat emptyRow) ++
  (take n $ repeat otherRow) ++
  (take (rows - 2*n) $ repeat emptyRow)
  where
    emptyRow = take cols $ repeat (0 :: Int)
    otherRow = map (\x -> if x > 5 && x < 10 then 1 else 0) [1..cols]
    n = rows `div` 3


showBoard :: [[Int]] -> String
showBoard board =
  concat $ map printRow board
  where
    printRow row = (show row) ++ "\n"


updateBoard :: [[Int]] -> [[Int]]
updateBoard board =
  map updateRow [0..(numRows-1)]
  where
    updateRow r = map (\c -> updateRC r c board)  [0..(numCols-1)]
    numRows = length board
    numCols = length $ board !! 0


updateRC :: Int -> Int -> [[Int]] -> Int
updateRC row col board =
  case s of
    2 -> board !! row !! col
    3 -> 1
    otherwise -> 0
  where
    numRows = length board
    numCols = length $ board !! 0
    offsets = [(row-1, col-1), (row-1, col), (row-1, col+1),
               (row,   col-1),               (row,   col+1),
               (row+1, col-1), (row+1, col), (row+1, col+1)]  
    offsets1 = map (\(r, c) -> (r `mod` numRows, c `mod` numCols)) offsets
    vals = map (\(r, c) -> board !! r !! c) offsets1
    s = sum vals

