-- xpr-fix.hs - xpr-fix in Haskell
--
-- The following parsers are implemented using parser combinators.
--
-- Copyright (C) 2022 Robert Coffey
-- Released under the MIT license.

import Control.Applicative
import Data.Char
import System.Environment

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

preE :: Parser String
preE =
  do o <- op
     some space
     l <- preE
     some space
     r <- preE
     return ("(" ++ l ++ " " ++ [o] ++ " " ++ r ++ ")")
   <|> num

-- Postfix RR ------------------------------------------------------------------
--
--   E -> N N O

postE :: Parser String
postE = P (\inp -> parse postE' $ reverse inp)

postE' :: Parser String
postE' =
  do o <- op
     some space
     r <- postE'
     some space
     l <- postE'
     return ("(" ++ l ++ " " ++ [o] ++ " " ++ r ++ ")")
   <|> num

-- Infix RR --------------------------------------------------------------------
--
--   E -> E [+,-] T | T
--   T -> T [*,/] P | P
--   P -> ( E ) | N

inE :: Parser String
inE = P (\inp -> parse inE' $ reverse inp)

inE' :: Parser String
inE' =
  do t <- inT
     many space
     o <- predChar $ flip elem "+-"
     many space
     e <- inE'
     return ("(" ++ e ++ " " ++ [o] ++ " " ++ t ++ ")")
   <|> inT

inT :: Parser String
inT =
  do p <- inP
     many space
     o <- predChar $ flip elem "*/"
     many space
     t <- inT
     return ("(" ++ t ++ " " ++ [o] ++ " " ++ p ++ ")")
   <|> inP

inP :: Parser String
inP =
  do char ')'
     e <- inE'
     char '('
     return e
   <|> num

-- main ------------------------------------------------------------------------

main :: IO ()
main =
  do args <- getArgs
     let out = case args of
                 ["prefix", expr]  -> parse preE expr
                 ["infix", expr]   -> parse inE expr
                 ["postfix", expr] -> parse postE expr
                 otherwise         -> [("", ""), ("", "")]
     putStrLn $ case out of
                  []            -> "error: failed to parse input"
                  [x, y]        -> "error: invalid arguments"
                  [(str, "")]   -> str
                  [(str, rest)] -> "error: unparsed input remains"
