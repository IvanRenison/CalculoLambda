
import Control.Applicative
import Data.Char
import Data.Maybe

import Parser

{-
La gramatica del calculo lambda está definida por:

V → x | (V)(V) | λx(V)
x → ⟨Char⟩

-}

data LambdaExp =
    Var Char | App LambdaExp LambdaExp | Lambda Char LambdaExp
    deriving (Show, Eq)


parseVar :: Parser LambdaExp
parseVar = Parser $ \s -> do
    (v, xs) <- listToMaybe $ readsPrec 11 s
    return (Var v, xs)

parseApp :: Parser LambdaExp
parseApp = Parser $ \s -> do
    ('(':xs) <- return $ dropWhile isSpace s
    (e1, ys) <- runParser parseLambdaExp $ dropWhile isSpace xs
    (')':ys') <- return $ dropWhile isSpace ys
    ('(':ys'') <- return $ dropWhile isSpace ys'
    (e2, zs) <- runParser parseLambdaExp $ dropWhile isSpace ys''
    (')':zs') <- return $ dropWhile isSpace zs
    return (App e1 e2, zs')


parseLambda :: Parser LambdaExp
parseLambda = Parser $ \s -> do
    ('λ':xs) <- return $ dropWhile isSpace s
    (Var x, ys) <- runParser parseVar $ dropWhile isSpace xs
    ('(':ys') <- return $ dropWhile isSpace ys
    (e, zs) <- runParser parseLambdaExp $ dropWhile isSpace ys'
    (')':zs') <- return $ dropWhile isSpace zs
    return (Lambda x e, zs')

parseLambdaExp :: Parser LambdaExp
parseLambdaExp = parseApp <|> parseLambda <|> parseVar


