{-# LANGUAGE GADTs #-}

module Colist where

import Later

data Colist a where
  Nil  :: Colist a
  Cons :: a -> Later (Colist a) -> Colist a

cat :: Colist a -> Colist a -> Colist a
cat = fix $
      \f xs ys -> case xs of
                    Nil -> ys
                    Cons x xs' -> Cons x (appL (appL f xs') (Next ys))

map :: (a -> b) -> Colist a -> Colist b
map f = fix $
        \g xs -> case xs of
                   Nil -> Nil
                   Cons x xs' -> Cons (f x) (appL g xs')