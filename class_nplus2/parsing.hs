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

char :: Char -> Parser Char
char x = satisfies (== x)

string :: Parser String
string = do
  char '"'
  xs <- many (do alphanum)
  char '"'
  return xs

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

int :: Parser Int
int = (do char '-'
          n <- nat
          return (-n)) <|> nat

