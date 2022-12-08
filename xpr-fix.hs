-- xpr-fix.hs - xpr-fix in Haskell
--
-- The following parsers are implemented using parser combinators.
--
-- Copyright (C) 2022 Robert Coffey
-- Released under the MIT license.

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

instance Functor Parser where
  fmap g p =
    P (\inp -> case parse p inp of
                 []         -> []
                 [(v, out)] -> [(g v, out)])

instance Applicative Parser where
  pure v = P (\inp -> [(v, inp)])

  pg <*> px =
    P (\inp -> case parse pg inp of
                 []         -> []
                 [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
  p >>= f =
    P (\inp -> case parse p inp of
                 []         -> []
                 [(v, out)] -> parse (f v) out)

instance Alternative Parser where
  empty = P (\inp -> [])

  p <|> q =
    P (\inp -> case parse p inp of
                 []        -> parse q inp
                 [(v, vs)] -> [(v, vs)])

item :: Parser Char
item = P (\inp -> case inp of
                    []     -> []
                    (x:xs) -> [(x, xs)])

predChar :: (Char -> Bool) -> Parser Char
predChar p = do x <- item
                if p x then return x else empty

char :: Char -> Parser Char
char x = predChar (== x)

digit :: Parser Char
digit = predChar isDigit

num :: Parser String
num = some digit

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

op :: Parser Char
op = predChar $ flip elem "+-*/"

space :: Parser Char
space = predChar isSpace

-- Prefix LL -------------------------------------------------------------------
--
--   E -> O N N

prefix :: Parser String
prefix =
  do o <- op
     some space
     l <- prefix
     some space
     r <- prefix
     return ("(" ++ l ++ " " ++ [o] ++ " " ++ r ++ ")")
   <|> num

-- Postfix RR ------------------------------------------------------------------
--
-- E -> N N O

postfix :: Parser String
postfix = P (\inp -> parse postfix' (reverse inp))

postfix' :: Parser String
postfix' =
  do o <- op
     some space
     r <- postfix'
     some space
     l <- postfix'
     return ("(" ++ l ++ " " ++ [o] ++ " " ++ r ++ ")")
   <|> num
