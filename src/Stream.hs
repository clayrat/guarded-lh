{-# LANGUAGE GADTs #-}

module Stream where

import Later

data Stream a where
  Cons :: a -> Later (Stream a) -> Stream a

head :: Stream a -> a
head (Cons x _) = x

tail :: Stream a -> Later (Stream a)
tail (Cons _ s') = s'

forever :: a -> Stream a
forever x = fix (Cons x)

map :: (a -> b) -> Stream a -> Stream b
map f = fix $ \g' s -> case s of
                         Cons x s' -> Cons (f x) (apL g' s')

foldr :: (a -> Later b -> b) -> Stream a -> b
foldr f = fix $ \g' s -> case s of
                           Cons x s' -> f x (apL g' s')

scanl1 :: (a -> a -> a) -> Stream a -> Stream a
scanl1 f = fix $ \g' s -> case s of
                            Cons x s' -> Cons x (mapL (Stream.map (f x)) (apL g' s'))

iterate :: Later (a -> a) -> a -> Stream a
iterate f = fix $ \g' x -> Cons x (apL g' (apL f (Next x)))

interleave :: Stream a -> Later (Stream a) -> Stream a
interleave = fix $ \g' s t' -> case s of
                                 Cons x s' -> Cons x (apL (apL g' t') (Next s'))

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f = fix $ \g' s t -> case (s, t) of
                               (Cons x s', Cons y t') -> Cons (f x y) (apL (apL g' s') t')

-- stream examples

nats :: Stream Int
nats = fix $ Cons 0 . mapL (Stream.map (+1))

fib :: Stream Int
fib = fix $ Cons 0 . mapL (\s -> Cons 1 (mapL (Stream.zipWith (+) s) (Stream.tail s)))

primes :: Stream Int
primes = fix $ Cons 2 . mapL (Stream.map (+1) . Stream.scanl1 (*))

toggle :: Stream Bool
toggle = fix $ Cons True . Next . Cons False

-- aka dragon curve
paperfolds :: Stream Bool
paperfolds = fix $ interleave toggle

thuemorse :: Stream Bool
thuemorse = fix $ Cons False . mapL (\tm -> Cons True (mapL go (Stream.tail (go tm))))
  where
  go :: Stream Bool -> Stream Bool
  go = fix $ \g' s -> case s of
                        Cons False s' -> Cons False (Next (Cons True (apL g' s')))
                        Cons True  s' -> Cons True (Next (Cons False (apL g' s')))

pascal :: Stream (Stream Int)
pascal = fix $ Cons (forever 1) . mapL (Stream.map go)
  where
  go :: Stream Int -> Stream Int
  go xs = fix $ Cons 1 . apL (mapL (Stream.zipWith (+)) (Stream.tail xs))

