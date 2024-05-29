{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnicodeSyntax #-}

import Control.Applicative (Alternative ((<|>)))
import Data.Char (isAlphaNum, isSpace)
import Data.List (union, (\\))
import Data.Maybe (maybeToList, fromMaybe)

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
isValidVarName xs = not (null xs) && all isAlphaNum xs && 'λ' `notElem` xs

allVarNames :: [String]
allVarNames = [x : xs | x ← ['a' .. 'z'], xs ← "" : allVarNames]

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
    y' = varNoEn $ x : variablesLibres_e

reducir :: LambdaExp → LambdaExp
reducir (Var x) = Var x
reducir (App (Lambda x e1) e2) = reducir $ remplazar x e2 e1
reducir (App e1 e2) = case reducir e1 of
  Lambda x e → reducir $ remplazar x e2 e
  e1' → App e1' $ reducir e2
reducir (Lambda x e) = Lambda x $ reducir e

redNorm :: LambdaExp → LambdaExp
redNorm (Var x) = Var x
redNorm (App (Lambda x e1) e2) = redNorm $ remplazar x e2 e1
redNorm (App e1 e2) = redNorm $ App (redNorm e1) e2
redNorm (Lambda x e) = Lambda x e

remplazarF :: (String → Maybe LambdaExp) → LambdaExp → LambdaExp
remplazarF f (Var x) = fromMaybe (Var x) $ f x
remplazarF f (App e1 e2) = App (remplazarF f e1) (remplazarF f e2)
remplazarF f (Lambda x e) = Lambda x' $ remplazarF f $ remplazar x (Var x') e
  where
    variablesLibres_e = variablesLibres e
    variablesLibres_f = variablesLibres_e >>= maybeToList . f >>= variablesLibres
    x' :: String
    x' = varNoEn $ x : variablesLibres_f ++ variablesLibres_e

nuevoContexto :: [(String, String)] → String → Maybe LambdaExp
nuevoContexto ctx x = fst <$> (lookup x ctx >>= runParser parseLambdaExp)

contextoBools :: String → Maybe LambdaExp
contextoBools = nuevoContexto [
    ("true", "λx y . x"),
    ("false", "λx y . y"),
    ("not", "λb x y . b y x"),
    ("and", "λb c x y . b (c x y) y"),
    ("or", "λb c x y . b x (c x y)"),
    ("if", "λb x y . b x y")
  ]

lambdaOfNat :: Word → LambdaExp
lambdaOfNat n = Lambda "f" $ Lambda "x" $ aux n
  where
    aux :: Word → LambdaExp
    aux 0 = Var "x"
    aux n = App (Var "f") $ aux (n - 1)

contextoNatNums :: String → Maybe LambdaExp
contextoNatNums x = case reads x of
  [(n, "")] → Just $ lambdaOfNat n
  _ → Nothing

contextoNatOps :: String → Maybe LambdaExp
contextoNatOps = nuevoContexto [
    ("succ", "λn f x . f (n f x)"),
    ("pred", "λn f x . n (λg h . h (g f)) (λu . x) (λu . u)"),
    ("add", "λm n f x . m f (n f x)"),
    ("sub", "λm n . n (λn f x . n (λg h . h (g f)) (λu . x) (λu . u)) m"),
    ("mul", "λm n f . m (n f)"),
    ("exp", "λm n . n m")
  ]

contextoNats :: String → Maybe LambdaExp
contextoNats x = contextoNatOps x <|> contextoNatNums x

contextoFuns :: String → Maybe LambdaExp
contextoFuns = nuevoContexto [
    ("Δ", "λ x . x x"),
    ("ΔΔ", "(λ x . x x) λ x . x x")
  ]

contextoUsual :: String → Maybe LambdaExp
contextoUsual x = contextoBools x <|> contextoNats x <|> contextoFuns x

-- Esto puede explotar
unsafeMagic :: String → LambdaExp
unsafeMagic x = reducir $ remplazarF contextoUsual $ fst . head $ maybeToList $ runParser parseLambdaExp x
