{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnicodeSyntax #-}

import Prelude.Unicode
import Data.List.Unicode

import Control.Applicative ( Alternative((<|>)) )
import Data.Char
import Data.List
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




variablesLibres :: LambdaExp → [Char]
variablesLibres (Var x) = [x]
variablesLibres (App e1 e2) = variablesLibres e1 ∪ variablesLibres e2
variablesLibres (Lambda x e) = variablesLibres e \\ [x]

remplazar :: LambdaExp → Char → LambdaExp → LambdaExp
remplazar e x (Var y)
    | y == x = e
    | otherwise = Var y
remplazar e x (App e1 e2) = App (remplazar e x e1) (remplazar e x e2)
remplazar e x (Lambda y e')
    | y == x = Lambda y e'
    | y ∉ variablesLibres_e = Lambda y $ remplazar e x e'
    | otherwise = Lambda y' $ remplazar e x $ remplazar (Var y') y e'
    where
        variablesLibres_e = variablesLibres e
        varNoEn :: Char → [Char] → Char
        varNoEn a [] = a
        varNoEn a (b:bs)
            | a == b = varNoEn a' bs
            | otherwise = varNoEn a bs
            where
                a' :: Char
                a'
                    | isLetter (succ a) ∧ succ a ≠ 'λ' = succ a
                    | otherwise = succ $ succ a
        y' :: Char
        y' = varNoEn y $ x:variablesLibres_e


reducir :: LambdaExp → LambdaExp
reducir (Var x) = Var x
reducir (App (Lambda x e1) e2) = reducir $ remplazar e2 x e1
reducir (App e1 e2) = reducir $ App (reducir e1) (reducir e2)
reducir (Lambda x e) = Lambda x $ reducir e


