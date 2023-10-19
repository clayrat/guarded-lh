{-# LANGUAGE GADTs #-}

module Conat where

import Later
import Stream

data Conat where
  Z :: Conat
  S :: Later Conat -> Conat

infinity :: Conat
infinity = lfix S

inc :: Conat -> Conat
inc = S . pure

isZero :: Conat -> Bool
isZero  Z    = True
isZero (S _) = False

-- discriminating predecessor
predM :: Conat -> Maybe (Later Conat)
predM  Z    = Nothing
predM (S c) = Just c

-- saturating predecessor
pred0 :: Conat -> Later Conat
pred0  Z    = pure Z
pred0 (S c) = c

-- return the number of iterations before hitting Nothing
unfold :: (a -> Maybe a) -> a -> Conat
unfold f = lfix $ \u' b -> case f b of
                             Nothing -> Z
                             Just a  -> S (u' <*> pure a)

{-@ fromNat :: Nat -> Conat @-}
fromNat :: Int -> Conat
fromNat n | n == 0    = Z
          | otherwise = inc (fromNat (n-1))

-- return the first number satisfying the predicate
{-@ search :: (Nat -> Bool) -> Conat @-}
search :: (Int -> Bool) -> Conat
search f = unfold (\n -> if f n then Nothing else Just (n+1)) 0

-- "pad" the number with infinite false bits
toStream :: Conat -> Stream Bool
toStream = lfix $ \ts' n -> case n of
                              Z    -> forever False
                              S n' -> Cons True (ts' <*> n')

-- TODO prove infinity = repeat True

-- interleaving style
add :: Conat -> Conat -> Conat
add = lfix $ \a' x y -> case (x, y) of
                          (Z   , Z   ) -> Z
                          (S x', Z   ) -> S x'
                          (Z   , S y') -> S y'
                          (S x', S y') -> S (pure (S (a' <*> x' <*> y')))

min :: Conat -> Conat -> Conat
min = lfix $ \m' x y -> case (x, y) of
                          (Z   , _)    -> Z
                          (S _ , Z)    -> Z
                          (S x', S y') -> S (m' <*> x' <*> y')

max :: Conat -> Conat -> Conat
max = lfix $ \m' x y -> case (x, y) of
                          (Z   , y)    -> y
                          (S x', Z)    -> S x'
                          (S x', S y') -> S (m' <*> x' <*> y')

-- concatenation style
addC :: Conat -> Conat -> Conat
addC x = lfix $ \a' y -> case y of
                           Z    -> x
                           S y' -> S (a' <*> y')

