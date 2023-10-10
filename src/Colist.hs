{-# LANGUAGE GADTs #-}

module Colist where

import Later

data Colist a where
  Nil  :: Colist a
  Cons :: a -> Later (Colist a) -> Colist a

forever :: a -> Colist a
forever x = lfix (Cons x)

cat :: Colist a -> Colist a -> Colist a
cat = lfix $
      \f xs ys -> case xs of
                    Nil -> ys
                    Cons x xs' -> Cons x (f <*> xs' <*> pure ys)

map :: (a -> b) -> Colist a -> Colist b
map f = lfix $
        \g xs -> case xs of
                   Nil -> Nil
                   Cons x xs' -> Cons (f x) (g <*> xs')