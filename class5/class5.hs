-------------------------------------------------------------
-- Lets make a new algebraic data type and make it a Monad --
-------------------------------------------------------------

-- Here we just want to make sure that we see the syntax. There are
-- some infix functions, with goofy anmes, e.g. <*>, but nothing deep.

-- Also, if an ADT has a single value constructor with one field, we
-- can use the keyword newtype instead of data.
newtype MyType a = MyType a

-- Here's function that puts something into MyType
putIntoMyType :: a -> MyType a
putIntoMyType x = MyType x

-- This will act on something in MyType with a function that puts the
-- result back.
actMonadically :: MyType a -> (a -> MyType b) -> MyType b
actMonadically (MyType a) f = f a

-- Everything up to here is stuff we've seen before.
-- Now we should make MyType instances of some typeclasses. 

-- For something to be a Monad it must be an Applicative. For
-- something to be an Applicative it must be a Functor.
instance Functor MyType where
  fmap f (MyType a) =  MyType $ f a

instance Applicative MyType where
  pure = putIntoMyType
  (<*>) (MyType f) (MyType a) = MyType $ f a

instance Monad MyType where
  (>>=) = actMonadically


-------------------------
-- Lets build a logger --
-------------------------

-- Take 1: We'll just append a log to our arguments, and the function
-- will concatenate

logAdd x y log = (value, newLog)
  where
    value = x + y
    newLog = log ++ " added " ++ (show x) ++ " and " ++ (show y) ++ ","

logSub x y log = (value, newLog)
  where
    value = x - y
    newLog = log ++ " subtracted " ++ (show y) ++ " from " ++ (show x) ++ ","

-- Lets compute 4 + 5 - 3 + 1

(v0, l0) = logAdd 4 5 ""
(v1, l1) = logSub v0 3 l0
(v2, l2) = logAdd v1 1 l1

-- Assessment: Composition is not so great.

-- Take 2: Maybe we can make composition easiser, and rethink our operations

numberWithLog' n = (n, "Starting with " ++ (show n))
logAdd' (x, log) y = (x + y, log ++ ", added " ++ (show y))
logSub' (x, log) y = (x - y, log ++ ", subtracted " ++ (show y))

(v3, l3)  = logAdd' (logSub' (logAdd' (numberWithLog' 4) 5) 3) 1 

-- Assessment: Composition is better, but all those parens are hard to
-- count. Also each operation, i.e. logAdd1, logSub1, needs to have
-- code to concatenate the new log and the previous log, can we factor
-- out this code?

-- Take 3: Lets see if Monads would work?

newtype LoggedNumber a = LoggedNumber (a, String) deriving (Show)

numberWithLog'' n = LoggedNumber (n, "")
logVal'' x = LoggedNumber (x, "The number is " ++ (show x))
logAdd'' y x = LoggedNumber (x + y, " plus " ++ (show y))
logSub'' y x = LoggedNumber (x - y, " minus " ++ (show y))

instance Functor LoggedNumber where
  fmap f (LoggedNumber (n, log)) =
    LoggedNumber (f n, log ++ " applied a function via fmap")

instance Applicative LoggedNumber where
  pure = numberWithLog''
  LoggedNumber (f, _) <*> x = fmap f x
  
-- As a Monad we'll use the bind operation to handle concatenating the
-- logging
instance Monad LoggedNumber where
  LoggedNumber (n, log) >>= f =
    let LoggedNumber (y, log1) = f n
    in LoggedNumber (y, log ++ " " ++ log1)

LoggedNumber (n, l) =
  numberWithLog'' 4 >>= logVal'' >>= logAdd'' 5 >>= logSub'' 3 >>= logAdd'' 1

-- Assessment: Not bad, but maybe there's a way to avoid using logVal.

---------------------
-- Lets do some IO --
---------------------

