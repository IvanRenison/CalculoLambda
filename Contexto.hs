{-# LANGUAGE UnicodeSyntax #-}

module Contexto where

import Lambda
import Parser
import Control.Applicative (Alternative((<|>)))
import Data.Maybe (maybeToList, listToMaybe)

nuevoContexto :: [(String, String)] → String → Maybe LambdaExp
nuevoContexto ctx x = remplazarF (nuevoContexto pre_ctx) . fst <$> (x_def >>= runParser parseLambdaExp)
  where
    (pre_ctx, pos_ctx) = span ((/= x) . fst) ctx
    x_def :: Maybe String
    x_def = snd <$> listToMaybe pos_ctx

contextoManual :: String → Maybe LambdaExp
contextoManual = nuevoContexto [
    ("Δ", "λ x . x x"),
    ("ΔΔ", "(λ x . x x) λ x . x x"),
    ("id", "λx . x"),
    ("rec", "λ f . (λ x . f (x x)) (λ x . f (x x))"),

    ("true", "λx y . x"),
    ("false", "λx y . y"),
    ("not", "λb x y . b y x"),
    ("and", "λb c . b c b"),
    ("or", "λb c . b b c"),
    ("xor", "λb c . b (not c) c"),
    ("eqBool", "λb c . b c (not c)"),
    ("if", "λb x y . b x y"),

    ("succ", "λn f x . f (n f x)"),
    ("pred", "λn f x . n (λg h . h (g f)) (λu . x) id"),
    ("add", "λm n f x . m f (n f x)"),
    ("sub", "λm n . n pred m"),
    ("mul", "λm n f . m (n f)"),
    ("exp", "λm n . n m"),
    ("isZero", "λn . n (λa . false) true"),
    ("le", "λn m . isZero (sub n m)"),
    ("eqNat", "λ n m . and (le n m) (le m n)"),
    ("neNat", "λ n m . not (eqNat n m)")
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

contextoUsual :: String → Maybe LambdaExp
contextoUsual x = contextoManual x <|> contextoNatNums x

-- Esto puede explotar
unsafeMagic :: String → LambdaExp
unsafeMagic x = reducir $ remplazarF contextoUsual $ fst . head $ maybeToList $ runParser parseLambdaExp x
