{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnicodeSyntax #-}

module Parser where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Char (isLetter, isSpace)

newtype Parser a = Parser { runParser :: String → Maybe (a, String) }

instance Functor Parser where
  fmap :: (a → b) → Parser a → Parser b
  fmap f (Parser p) = Parser $ \s → do
    (a, s') ← p s
    return (f a, s')

instance Applicative Parser where
  pure :: a → Parser a
  pure a = Parser $ \s → return (a, s)

  (<*>) :: Parser (a → b) → Parser a → Parser b
  (Parser f) <*> (Parser a) = Parser $ \s → do
    (f', s') ← f s
    (a', s'') ← a s'
    return (f' a', s'')

instance Monad Parser where
  (>>=) :: Parser a → (a → Parser b) → Parser b
  (Parser p) >>= f = Parser $ \s → do
    (a, s') ← p s
    runParser (f a) s'

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing

  (<|>) :: Parser a → Parser a → Parser a
  (Parser p) <|> (Parser q) = Parser $ \s → p s <|> q s

parseChar :: Char → Parser Char
parseChar c = Parser $ \case
  [] → Nothing
  (x:xs) → if x == c then Just (c, xs) else Nothing

parseLetter :: Parser Char
parseLetter = Parser $ \case
  [] → Nothing
  (x:xs) → if isLetter x then Just (x, xs) else Nothing

parseWord :: Parser String
parseWord = do
  c ← parseLetter
  cs ← parseWord <|> pure []
  return (c:cs)

skipSpaces :: Parser ()
skipSpaces = Parser $ \s → return ((), dropWhile isSpace s)

parseInParentheses :: Parser a → Parser a
parseInParentheses p = do
  parseChar '('
  skipSpaces
  a ← p
  skipSpaces
  parseChar ')'
  return a
