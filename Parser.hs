{-# LANGUAGE InstanceSigs #-}

module Parser where

import Control.Applicative

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $ \s -> do
        (a, s') <- p s
        return (f a, s')

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = Parser $ \s -> return (a, s)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser f) <*> (Parser a) = Parser $ \s -> do
        (f', s') <- f s
        (a', s'') <- a s'
        return (f' a', s'')

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser p) >>= f = Parser $ \s -> do
        (a, s') <- p s
        runParser (f a) s'

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ \_ -> Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser p) <|> (Parser q) = Parser $ \s -> p s <|> q s


parseChar :: Char -> Parser Char
parseChar c = Parser $ \s ->
    case s of
        [] -> Nothing
        (x:xs) -> if x == c then Just (c, xs) else Nothing

