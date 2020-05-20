import Control.Applicative
import Data.Char
import Debug.Trace (trace)

newtype Parser a = P (String -> [(a, String)])

parse :: (Parser a) -> String -> [(a, String)]
parse (P p) input = p input

instance Functor Parser where
  -- fmap (a -> b) -> f a -> f b
  fmap g p = P (\input -> case parse p input of
                   [] -> []
                   [(v, remainder)] -> [(g v, remainder)])

instance Applicative Parser where
  -- pure :: a -> f a
  pure v = P (\input -> [(v, input)])

  -- <*> :: f (a -> b) -> f a -> f b
  pg <*> px = P (\input -> case parse pg input of
                    [] -> []
                    [(g, remainder)] -> parse (fmap g px) remainder)

instance Monad Parser where
  -- (>>=) :: m a -> (a -> m b) -> mb
  p >>= f = P (\input -> case parse p input of
                  [] -> []
                  [(v, remainder)] -> parse (f v) remainder)
                   
instance Alternative Parser where
  -- empty :: m a
  empty = P (\input -> [])

  -- <|> :: Parse a -> Parser a -> Parser a
  p <|> q = P (\input -> case parse p input of
                  [] -> parse q input
                  [(v, remainder)] -> [(v, remainder)])

item :: Parser Char
item = P (\input -> case input of
             [] -> []
             (x:xs) -> [(x, xs)])

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = do
  x <- item
  if p x
    then return x
    else empty

digit, lower, upper, letter, alphanum :: Parser Char
digit = satisfies isDigit
lower = satisfies isLower
upper = satisfies isUpper
letter = satisfies isAlpha
alphanum = satisfies isAlphaNum
space = satisfies isSpace
notQuote = satisfies ((/=) '"')

-- Parse a string literal, e.g. "This is a string literal"
char :: Char -> Parser Char
char x = satisfies ((==) x)

stringLiteral :: Parser String
stringLiteral = do
  char '"'
  xs <- many (do notQuote)
  char '"'
  return xs


-- Parse natural numbers and integers.
nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int0 = do char '-'
          n <- nat
          return (-n)

int1 = nat

int :: Parser Int
int = int1 <|> int0

-- Parse floating point numbers
float1, float3 :: Parser Float
float1 = do f <- float3
            char 'e'
            e <- int
            let go x e =
                  if e == 0
                  then x
                  else if e > 0
                       then go (x*10) (e-1)
                       else go (x/10) (e+1)
            return $ go f e

float2 :: Parser Float
float2 = do n <- int
            let f = fromIntegral n
            char 'e'
            e <- int
            let go x e =
                  if e == 0
                  then x
                  else if e > 0
                       then go (x*10) (e-1)
                       else go (x/10) (e+1)
            return $ go f e

float3 = do n1 <- int
            char '.'
            n2 <- nat
            let go x =
                  if x < 1
                  then x
                  else go $ x/10
            return $ fromIntegral n1 + (go $ fromIntegral n2)
           
float4 :: Parser Float
float4 = do n <- int
            return $ fromIntegral n

float :: Parser Float
float = float1 <|> float2 <|> float3 <|> float4

keyword0 :: String -> Parser String
keyword0 [] = return []
keyword0 (x:xs) = do char x
                     keyword0 xs
                     return (x:xs)

-- Parse a name, like a string without quotes.
name0 :: Parser String
name0 = do x <- letter
           xs <- many alphanum
           return (x:xs)

-- Parse some spaces and return an empty item 
spaces :: Parser ()
spaces = do many space
            return ()

-- Creata a parser that allows for spaces before and after the desired
-- item.
token :: Parser a -> Parser a
token p = spaces *> p <* spaces 

-- Use the above to parse integers...
integer :: Parser Int
integer = token int

-- ... floating point numbers
floating :: Parser Float
floating = token float

-- ... specific keywords, only match if the specific string is
-- present followed by at least one space..
keyword :: String -> Parser String
keyword s = do spaces
               keyword0 s
               space
               return (s)

symbol s = do spaces
              keyword0 s
              spaces
              return (s)
  

-- ... names of things. 
name :: Parser String
name = token name0

-- An ADT that is a sum type (over Book and Thing) of two product types (Book, Thing)

data Thing =
  Book { title :: String
       , author :: String
       , numPages :: Int
       , price :: Float
       } |
  Movie { title :: String
        , runningTimeMinutes :: Int
        , rating :: Char
        , yearReleased :: Int
        } deriving (Show)

pBook :: Parser Thing
pBook = pure Book <*> (token stringLiteral) <*> (token stringLiteral) <*> integer <*> floating

pMovie :: Parser Thing
pMovie = pure Movie <*> (token stringLiteral) <*> integer <*> (token item) <*> integer

pThing :: Parser Thing
pThing = pBook <|> pMovie

pThings :: Parser [Thing]
pThings = do (many pThing)

-- Examples from G. Hutton

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t+e)
           <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
           <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          <|> integer

eval :: String -> Int
eval xs = case (parse expr xs) of
            [(n, [])] -> n
            [(_, out)] -> error ("Unused input " ++ out)
            [] -> error "Invalid input"
                               
  
