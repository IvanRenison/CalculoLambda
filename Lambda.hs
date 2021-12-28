{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnicodeSyntax #-}

import Prelude.Unicode

import Control.Applicative ( Alternative((<|>)) )
import Data.Char
import Data.Maybe

import Parser

{-
La gramatica del calculo lambda está definida por:

V → x | (VV) | (λxV)
x → ⟨Char⟩

-}

data LambdaExp =
    Var Char | App LambdaExp LambdaExp | Lambda Char LambdaExp
    deriving (Eq)

instance Show LambdaExp where
    show (Var x) = [x]
    show (App e1 e2) = "(" ++ show e1 ++ show e2 ++ ")"
    show (Lambda x e) = "(λ" ++ [x] ++ show e ++ ")"

parseVar :: Parser LambdaExp
parseVar = Parser $ \case
    [] → Nothing
    (x:xs) →
        if isAlpha x ∧ x ≠ 'λ'
            then Just (Var x, xs)
            else Nothing

parseApp :: Parser LambdaExp
parseApp = Parser $ \s → do
    ('(':xs) ← return $ dropWhile isSpace s
    (e1, ys) ← runParser parseLambdaExp $ dropWhile isSpace xs
    (e2, zs) ← runParser parseLambdaExp $ dropWhile isSpace ys
    (')':zs') ← return $ dropWhile isSpace zs
    return (App e1 e2, zs')


parseLambda :: Parser LambdaExp
parseLambda = Parser $ \s → do
    ('(':xs) ← return $ dropWhile isSpace s
    ('λ':xs') ← return $ dropWhile isSpace xs
    (Var x, ys') ← runParser parseVar $ dropWhile isSpace xs'
    (e, zs) ← runParser parseLambdaExp $ dropWhile isSpace ys'
    (')':zs') ← return $ dropWhile isSpace zs
    return (Lambda x e, zs')

parseLambdaExp :: Parser LambdaExp
parseLambdaExp = parseApp <|> parseLambda <|> parseVar

instance Read LambdaExp where
    readsPrec _ = maybeToList . runParser parseLambdaExp

