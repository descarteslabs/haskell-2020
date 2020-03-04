-- Problem 1

fact n = n * (fact $ n - 1)

factTR n = go n 1
  where go 1 b = b
        go a b = go (a-1) (a*b)

-- Problem 2 (not really a solution)

fib n = fibs !! n
  where fibs = 1:1:zipWith (+) fibs (drop 1 fibs)

-- Problem 3

isConst [] = True
isConst xs = (init xs) == (tail xs)

-- Problem 4

data MyFirstType = MyFirstValueConstructor Int deriving (Show)

data MyFirstTypeConstructor a = MySecondValueConstructor a deriving (Show)

instance Num MyFirstType where
  MyFirstValueConstructor x + MyFirstValueConstructor y = MyFirstValueConstructor (x + y)
  MyFirstValueConstructor x - MyFirstValueConstructor y = MyFirstValueConstructor (x - y)
  MyFirstValueConstructor x * MyFirstValueConstructor y = MyFirstValueConstructor (x * y)
  abs (MyFirstValueConstructor x) = MyFirstValueConstructor (Prelude.abs x)
  signum (MyFirstValueConstructor x) = MyFirstValueConstructor (Prelude.signum x)
  fromInteger x = MyFirstValueConstructor (Prelude.fromInteger x)

instance (Num a) => Num (MyFirstTypeConstructor a) where
  MySecondValueConstructor x + MySecondValueConstructor y = MySecondValueConstructor (x + y)
  MySecondValueConstructor x - MySecondValueConstructor y = MySecondValueConstructor (x - y)
  MySecondValueConstructor x * MySecondValueConstructor y = MySecondValueConstructor (x * y)
  abs (MySecondValueConstructor x) = MySecondValueConstructor (Prelude.abs x)
  signum (MySecondValueConstructor x) = MySecondValueConstructor (Prelude.signum x)
  fromInteger x = MySecondValueConstructor (Prelude.fromInteger x)
  

-- Page 2: Problem 1

data Book = Book {
    title :: String
  , author :: String
  , yearPublished :: Int
  , cost :: Float
  } deriving (Eq)

instance Show Book where
  show b =
    "Title: " ++ (title b) ++
    " by " ++ (author b) ++
    " published " ++ (show $ yearPublished b) ++
    " for only $" ++ (show $ cost b)

-- Page 2: Problem 2

data Boolish = Trueish | Falseish | Unknown

instance Eq Boolish where
  Trueish == Trueish = True
  Falseish == Falseish = True
  Unknown == Unknown = True
  _ == _ = False
