{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnicodeSyntax #-}

import Control.Applicative (Alternative ((<|>)))
import Data.Char (isLetter, isSpace)
import Data.List (union, (\\))
import Data.Maybe (maybeToList)
import Parser

{-
La gramática del calculo lambda está definida por:

⟨expr⟩ → ⟨var⟩ | (⟨expr⟩⟨expr⟩) | (λ⟨var⟩.⟨expr⟩)

-}

data LambdaExp
  = Var String
  | App LambdaExp LambdaExp
  | Lambda String LambdaExp
  deriving (Eq)

multiApp :: [LambdaExp] → LambdaExp
multiApp [] = undefined
multiApp [e] = e
multiApp [e0, e1] = App e0 e1
multiApp (e : es) = App e $ multiApp es

multiLambda :: [String] → LambdaExp → LambdaExp
multiLambda xs e = foldr Lambda e xs

instance Show LambdaExp where
  show (Var x) = x
  show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Lambda x e) = "(λ" ++ x ++ "." ++ show e ++ ")"

parseVarLetter :: Parser Char
parseVarLetter = Parser $ \case
  [] → Nothing
  (x : xs) → if isLetter x && x /= 'λ' then Just (x, xs) else Nothing

parseVar :: Parser String
parseVar = do
  c ← parseVarLetter
  cs ← parseVar <|> pure []
  return (c : cs)

parseVarExpr :: Parser LambdaExp
parseVarExpr = do
  skipSpaces
  Var <$> parseVar

parseApps :: Parser [LambdaExp]
parseApps = do
  skipSpaces
  e ← parseInParentheses parseLambdaExp <|> parseVarExpr <|> parseLambda
  es ← parseApps <|> pure []
  return (e : es)

parseApp :: Parser LambdaExp
parseApp = do
  multiApp <$> parseApps

parseVarList :: Parser [String]
parseVarList = do
  skipSpaces
  x ← parseVar
  xs ← parseVarList <|> pure []
  return (x : xs)

parseLambda :: Parser LambdaExp
parseLambda = Parser $ \s → do
  ('λ' : xs) ← return $ dropWhile isSpace s
  (vars, ys) ← runParser parseVarList xs
  ('.' : zs) ← return $ dropWhile isSpace ys
  (e, ws) ← runParser parseLambdaExp $ dropWhile isSpace zs
  return (multiLambda vars e, ws)

parseLambdaExp :: Parser LambdaExp
parseLambdaExp =
  parseApp
  <|> parseLambda
  <|> parseVarExpr
  <|> parseInParentheses parseLambdaExp

instance Read LambdaExp where
  readsPrec _ = maybeToList . runParser parseLambdaExp
{-
variablesLibres :: LambdaExp → [Char]
variablesLibres (Var x) = [x]
variablesLibres (App e1 e2) = variablesLibres e1 `union` variablesLibres e2
variablesLibres (Lambda x e) = variablesLibres e \\ [x]

remplazar :: LambdaExp → Char → LambdaExp → LambdaExp
remplazar e x (Var y)
  | y == x = e
  | otherwise = Var y
remplazar e x (App e1 e2) = App (remplazar e x e1) (remplazar e x e2)
remplazar e x (Lambda y e')
  | y == x = Lambda y e'
  | y `notElem` variablesLibres_e = Lambda y $ remplazar e x e'
  | otherwise = Lambda y' $ remplazar e x $ remplazar (Var y') y e'
  where
    variablesLibres_e = variablesLibres e
    varNoEn :: Char → [Char] → Char
    varNoEn a [] = a
    varNoEn a (b : bs)
      | a == b = varNoEn a' bs
      | otherwise = varNoEn a bs
      where
        a', a'' :: Char
        a'
          | isLetter a'' && a'' /= 'λ' = a''
          | otherwise = succ a''
        a'' = succ a
    y' :: Char
    y' = varNoEn y $ x : variablesLibres_e

reducir :: LambdaExp → LambdaExp
reducir (Var x) = Var x
reducir (App (Lambda x e1) e2) = reducir $ remplazar e2 x e1
reducir (App e1 e2) = reducir $ App (reducir e1) (reducir e2)
reducir (Lambda x e) = Lambda x $ reducir e
 -}