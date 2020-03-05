--------------------
-- Tail recursion --
--------------------

-- Not tail recursive because the returned expression is 1 +
-- <recursive call> so we need the result of the recursion before the
-- current stack frame can complete.
listLength [] = 0
listLength (x:xs) = 1 + listLength xs


-- Tail recursion, take 1:
--
-- We want to accumulate the length, so we create a wrapper function
-- that has the desired type signature and an implementation function
-- that has a type signature allowing for an accumulator.
listLength' xs = listLengthImpl xs 0

listLengthImpl [] n = n
listLengthImpl (x:xs) n = listLengthImpl xs (1 + n)


-- Tail recursion, take 2:
--
-- Because listLengthImpl is used in a single location, we can move it into a where-clause, and then we need not worry about giving the impl function a unique name.
listLength'' xs = go xs 0
  where
    go [] n = n
    go (x:xs) n = go xs (1 + n)


-- How do we make this tail recursive?
listSum [] = 0
listSum (x:xs) = x + listSum xs

listSum' xs = go xs 0
  where
    go [] n = n
    go (x:xs) n = go xs (x + n)


-- Can we make this tail-recursive? Not sure. 
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

--------------------------
-- Algebraic Data Types --
--------------------------

data MyFirstType = MyFirstValueConstructor Int

data MyFirstTypeConstructor a = MySecondValueConstructor a

-- Now is a good time to introduce kinds

data PerchanceAValue a = Yes a | No

-- Lets make our new types instances of the Show typeclass

instance Show MyFirstType where
  show (MyFirstValueConstructor i) = show i

-- Here we add a constraint on the type a
instance (Show a) => Show (MyFirstTypeConstructor a) where
  show (MySecondValueConstructor x) = show x 

myPrint :: (Show a) => a -> String
myPrint x = show x

instance (Show a) => Show (PerchanceAValue a) where
  show (Yes x) = show x
  show No = "No value"

-- -- Lets make our new types instance of the Eq typeclass

instance Eq MyFirstType where
  (==) (MyFirstValueConstructor i) (MyFirstValueConstructor j) = i == j

instance (Eq a) => Eq (MyFirstTypeConstructor a) where
  (==) (MySecondValueConstructor i) (MySecondValueConstructor j) = i == j

instance (Eq a) => Eq (PerchanceAValue a) where
  (==) (Yes x) (Yes j) = x == j
  (==) No No = True
  (==) _ _ = False


-- -- Lets try this out
pv1 = Yes 8
pv2 = Yes 9
pv3 = No
pv4 = Yes 8

test1 = pv1 == pv4
test2 = pv1 == pv2

-- -- The following is an example of "record syntax" and default
-- -- typeclass implementations. Note that "Book" is the name of the type
-- -- constructor and the value constructor.
-- --
-- -- Note the placement of commets -- this is a common Haskell idiom.

data Book = Book {
    title :: String
  , author :: String
  , yearPublished :: Int
  , cost :: Float
  } deriving (Show, Eq)

data Movie = Movie {
    title :: String
  , year :: Int
  } deriving (Show, Eq)

-- When using record syntax you get, for free, accessor functions
-- whose names are the same as the fields, e.g.

goodBook = Book "Learning Haskell at Work" "Winston and Matt" 2020 12.95

titleOfGoodBook = title goodBook

-- The function title has the following implementation

title' (Book t _ _ _) = t

-- The following, if we uncomment it, will generate an error message
-- as a duplicate function definition.

-- -- title (Book t _ _ _) = t

