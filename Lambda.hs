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

isValidVarName :: String → Bool
isValidVarName xs = not (null xs) && all isLetter xs && 'λ' `notElem` xs

allVarNames :: [String]
allVarNames = [x : xs | x ← ['a' .. 'z'], xs ← "" : allVarNames]

varNoEn :: [String] → String
varNoEn bs = head $ filter (`notElem` bs) allVarNames

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
    y' = varNoEn $ x : variablesLibres_e

redNorm :: LambdaExp → LambdaExp
redNorm (Var x) = Var x
redNorm (App (Lambda x e1) e2) = redNorm $ remplazar x e2 e1
redNorm (App e1 e2) = redNorm $ App (redNorm e1) e2
redNorm (Lambda x e) = Lambda x e

remplazarF :: (String -> LambdaExp) -> LambdaExp -> LambdaExp
remplazarF f (Var x) = f x
remplazarF f (App e1 e2) = App (remplazarF f e1) (remplazarF f e2)
remplazarF f (Lambda x e) = Lambda x' $ remplazarF f $ remplazar x (Var x') e
  where
    variablesLibres_e = variablesLibres e
    variablesLibres_f = concatMap (variablesLibres . f) variablesLibres_e
    x' :: String
    x' = varNoEn $ x : variablesLibres_f
