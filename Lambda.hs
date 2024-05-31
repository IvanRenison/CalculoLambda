{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnicodeSyntax #-}

module Lambda where

import Control.Applicative (Alternative ((<|>)))
import Data.Char (isAlphaNum, isSpace)
import Data.List (union, (\\))
import Data.Maybe (fromMaybe, maybeToList)

import Parser
import Text.XHtml (variable)

{-
La gramática del calculo lambda está definida por:

⟨expr⟩ → ⟨var⟩ | (⟨expr⟩⟨expr⟩) | (λ⟨var⟩.⟨expr⟩)

-}

data LambdaExp
  = Var String
  | App LambdaExp LambdaExp
  | Lambda String LambdaExp
  deriving (Eq)

isValidVarName :: String → Bool
isValidVarName xs = not (null xs) && all isAlphaNum xs && 'λ' `notElem` xs

allVarNames :: [String]
allVarNames = [x : xs | xs ← "" : allVarNames, x ← ['a' .. 'z']]

varNoEn :: [String] → String
varNoEn bs = head $ filter (`notElem` bs) allVarNames

multiApp :: [LambdaExp] → LambdaExp
multiApp es = multiApp' $ reverse es
  where
    multiApp' :: [LambdaExp] → LambdaExp
    multiApp' [] = undefined
    multiApp' [e] = e
    multiApp' (e : es) = App (multiApp' es) e

multiLambda :: [String] → LambdaExp → LambdaExp
multiLambda xs e = foldr Lambda e xs

instance Show LambdaExp where
  show (Var x) = x
  show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Lambda x e) = "(λ" ++ x ++ "." ++ show e ++ ")"

parseVarLetter :: Parser Char
parseVarLetter = Parser $ \case
  [] → Nothing
  (x : xs) → if isAlphaNum x && x /= 'λ' then Just (x, xs) else Nothing

parseVar :: Parser String
parseVar = do
  c ← parseVarLetter
  cs ← parseVar <|> pure []
  return (c : cs)

parseVarExpr :: Parser LambdaExp
parseVarExpr = do
  skipSpaces
  Var <$> parseVar

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

parseApps :: Parser [LambdaExp]
parseApps = do
  skipSpaces
  e ← parseInParentheses parseLambdaExp <|> parseVarExpr <|> parseLambda
  es ← parseApps <|> pure []
  return (e : es)

parseLambdaExp :: Parser LambdaExp
parseLambdaExp = do
  multiApp <$> parseApps

instance Read LambdaExp where
  readsPrec _ = maybeToList . runParser parseLambdaExp

variablesLibres :: LambdaExp → [String]
variablesLibres (Var x) = [x]
variablesLibres (App e1 e2) = variablesLibres e1 `union` variablesLibres e2
variablesLibres (Lambda x e) = variablesLibres e \\ [x]

remplazar :: String → LambdaExp → LambdaExp → LambdaExp
remplazar x e (Var y)
  | y == x = e
  | otherwise = Var y
remplazar x e (App e1 e2) = App (remplazar x e e1) (remplazar x e e2)
remplazar x e (Lambda y e')
  | y == x = Lambda y e'
  | y `notElem` variablesLibres_e = Lambda y $ remplazar x e e'
  | otherwise = Lambda y' $ remplazar x e $ remplazar y (Var y') e'
  where
    variablesLibres_e = variablesLibres e
    y' :: String
    y' = varNoEn $ x : variablesLibres_e ++ variablesLibres e'

reducirNormal :: LambdaExp → LambdaExp
reducirNormal (Var x) = Var x
reducirNormal (App e1 e2) = case reducirNormal e1 of
  Lambda x e → reducirNormal $ remplazar x e2 e
  e1' → App e1' $ reducirNormal e2
reducirNormal (Lambda x e) = Lambda x e

reducir :: LambdaExp → LambdaExp
reducir (Var x) = Var x
reducir (App e1 e2) = case reducirNormal e1 of
  Lambda x e → reducir $ remplazar x e2 e
  e1' → App e1' $ reducir e2
reducir (Lambda x e) = Lambda x $ reducir e

remplazarF :: (String → Maybe LambdaExp) → LambdaExp → LambdaExp
remplazarF f (Var x) = fromMaybe (Var x) $ f x
remplazarF f (App e1 e2) = App (remplazarF f e1) (remplazarF f e2)
remplazarF f (Lambda x e) = Lambda x' $ remplazarF f $ remplazar x (Var x') e
  where
    variablesLibres_e = variablesLibres e
    variablesLibres_f = variablesLibres_e >>= maybeToList . f >>= variablesLibres
    x' :: String
    x' = varNoEn $ x : variablesLibres_f ++ variablesLibres_e
