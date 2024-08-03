{-# OPTIONS_GHC -w #-}
-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

module Parsing.Parsing (module Parsing.Parsing, module Control.Applicative) where

import Control.Applicative
import Data.Char

-- Basic definitions

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

-- Sequencing parsers

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v,inp)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)

-- Making choices

instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\inp -> [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           [(v,out)] -> [(v,out)])

-- Derived primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many (alphanum <|> char '_')
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

-- Handling spacing

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

inQuotesString :: Parser String
inQuotesString = do 
    _ <- space
    _ <- char '"'
    x <- many (sat (/= '\"'))
    _ <- char '"'
    _ <- space
    return x

notQuote :: Parser Char
notQuote = sat (\ x -> x /= '\"' && x /= '\\') <|> escapedChars

escapedChars :: Parser Char
escapedChars = do
   _ <- char '\\'
   nxt <- item
   case nxt of
      '"' -> return '\"'
      '\\' -> return '\\'
      '/' -> return '/'
      'f' -> return '\f'
      'b' -> return '\b'
      'n' -> return '\n'
      'r' -> return '\r'
      't' -> return '\t'
      'u' -> readHexNum

readHexNum :: Parser Char
readHexNum = do
   d1 <- alphanum
   d2 <- alphanum
   d3 <- alphanum
   d4 <- alphanum
   if isHexDigit d1 && isHexDigit d2 && isHexDigit d3 && isHexDigit d4
      then return $ chr $ read ("0x" ++ [d1, d2, d3, d4])
      else empty



frac1 :: Parser Double
frac1 = do
   x <- some digit
   char '.'
   y <- many digit
   return (read (x ++ "." ++ y))
   
frac2 :: Parser Double
frac2 = do
   x <- many digit
   char '.'
   y <- some digit
   return (read (x ++ "." ++ y))

fractional :: Parser Double
fractional = frac1 <|> frac2

scientific :: Parser Double
scientific = do
   x <- fractional <|> doubleNum
   _ <- char 'e' <|> char 'E'
   _ <- many (sat (== '+'))
   exp <- int
   return (x * (10 ^^ exp))

readPosDoubleNumber :: Parser Double
readPosDoubleNumber = scientific <|> fractional <|> doubleNum

readNegDoubleNumber :: Parser Double
readNegDoubleNumber = do
   _ <- char '-'
   n <- readPosDoubleNumber
   return (-n)

readDoubleNumber :: Parser Double
readDoubleNumber = readPosDoubleNumber <|> readNegDoubleNumber

doubleNum :: Parser Double
doubleNum = do
   x <- some digit
   return (read x)
