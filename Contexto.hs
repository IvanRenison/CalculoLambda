{-# LANGUAGE UnicodeSyntax #-}

module Contexto where

import Lambda
import Parser
import Control.Applicative (Alternative((<|>)))
import Data.Maybe (maybeToList)

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
    ("exp", "λm n . n m"),
    ("isZero", "λn . n (λa x y . y) (λx y . x)"),
    ("eq", "")
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
