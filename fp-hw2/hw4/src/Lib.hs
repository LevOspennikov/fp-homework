{-# LANGUAGE InstanceSigs #-}

module Lib  where


import           Control.Applicative
import           Data.Char


newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f []          = Nothing
    f (x:xs)
      | p x       = Just (x, xs)
      | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs


instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser m
    where
      m s = first f <$> (runParser p s)


first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (\_ -> Just (x,[]))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) p secondP = Parser m
    where
      m s = case runParser p s of
              Nothing     -> Nothing
              Just (f, c) -> first f <$> runParser secondP c

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = const . const () <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair =  (\int _ int' -> [int,int']) <$> posInt <*> char ' ' <*> posInt
