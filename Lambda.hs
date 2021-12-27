import Control.Applicative
import Data.Maybe

import Parser

{-
La gramatica del calculo lambda está definida por:

V → x | (V)(V) | λx(V)
x → ⟨Char⟩

-}

data LambdaExp =
    Var String | App LambdaExp LambdaExp | Lambda String LambdaExp
    deriving (Show, Eq)


parseVar :: Parser LambdaExp
parseVar = Parser $ \s -> do
    (v, xs) <- listToMaybe $ readsPrec 11 s
    return (Var v, xs)

parseApp :: Parser LambdaExp
parseApp = undefined

parseLambda :: Parser LambdaExp
parseLambda = undefined


parseLambdaExp :: Parser LambdaExp
parseLambdaExp = parseVar <|> parseApp <|> parseLambda


