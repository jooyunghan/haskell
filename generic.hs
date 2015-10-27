{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving, FlexibleContexts #-}

module Main where

data List a = Nil | Cons a (List a) deriving Show

cons a as = Cons a as
nil = Nil

data ListF a r = NilF | ConsF a r deriving Show

data Fix f = In (f (Fix f))

deriving instance (Show (f (Fix f))) => Show (Fix f)

type List' a = Fix (ListF a)


one :: List' Integer
one = In (ConsF 1 (In NilF))

main = do
  print $ cons 1 nil
