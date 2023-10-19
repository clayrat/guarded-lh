{-# LANGUAGE GADTs #-}

module Colist where

import Later

data Colist a where
  Nil  :: Colist a
  Cons :: a -> Later (Colist a) -> Colist a

consL :: a -> Colist a -> Colist a
consL x xs = Cons x (pure xs)

singleton :: a -> Colist a
singleton a = consL a Nil

repeat :: a -> Colist a
repeat x = lfix (Cons x)

uncons :: Colist a -> Maybe (a, Later (Colist a))
uncons  Nil         = Nothing
uncons (Cons x xs') = Just (x, xs')

fromList :: [a] -> Colist a
fromList []     = Nil
fromList (x:xs) = consL x (fromList xs)

cat :: Colist a -> Colist a -> Colist a
cat = lfix $
      \c' xs ys -> case xs of
                     Nil -> ys
                     Cons x xs' -> Cons x (c' <*> xs' <*> pure ys)

map :: (a -> b) -> Colist a -> Colist b
map f = lfix $
        \m' xs -> case xs of
                    Nil -> Nil
                    Cons x xs' -> Cons (f x) (m' <*> xs')

zipWith :: (a -> b -> c) -> Colist a -> Colist b -> Colist c
zipWith f = lfix $
            \z' xs ys -> case (xs , ys) of
                           (Nil       , _         ) -> Nil
                           (Cons _ _  , Nil       ) -> Nil
                           (Cons x xs', Cons y ys') -> Cons (f x y) (z' <*> xs' <*> ys')

prependToAll :: a -> Colist a -> Colist a
prependToAll sep = lfix $ \p' xs -> case xs of
                                      Nil -> Nil
                                      Cons x xs' -> consL x (Cons x (p' <*> xs'))

intersperse :: a -> Colist a -> Colist a
intersperse sep  Nil         = Nil
intersperse sep (Cons x xs') = Cons x (fmap (prependToAll sep) xs')

foldr :: (a -> Later b -> b) -> Colist a -> b -> b
foldr f c z = lfix (\f' xs -> case xs of
                                Nil -> z
                                Cons x xs' -> f x (f' <*> xs')) c