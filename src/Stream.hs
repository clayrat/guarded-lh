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
forever x = lfix (Cons x)

map :: (a -> b) -> Stream a -> Stream b
map f = lfix $ \g' s -> case s of
                          Cons x s' -> Cons (f x) (g' <*> s')

foldr :: (a -> Later b -> b) -> Stream a -> b
foldr f = lfix $ \g' s -> case s of
                            Cons x s' -> f x (g' <*> s')

scanl1 :: (a -> a -> a) -> Stream a -> Stream a
scanl1 f = lfix $ \g' s -> case s of
                             Cons x s' -> Cons x ((Stream.map (f x)) <$> (g' <*> s'))

iterate :: Later (a -> a) -> a -> Stream a
iterate f = lfix $ \g' x -> Cons x (g' <*> (f <*> pure x))

iterate' :: (a -> a) -> a -> Stream a
iterate' f = Stream.iterate (pure f)

interleave :: Stream a -> Later (Stream a) -> Stream a
interleave = lfix $ \g' s t' -> case s of
                                  Cons x s' -> Cons x (g' <*> t' <*> pure s')

interleave' :: Stream a -> Stream a -> Stream a
interleave' a b = interleave a (pure b)

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f = lfix $ \g' s t -> case (s, t) of
                                (Cons x s', Cons y t') -> Cons (f x y) (g' <*> s' <*> t')

zip :: Stream a -> Stream b -> Stream (a ,b)
zip = Stream.zipWith (,)

-- stream examples

nats :: Stream Int
nats = lfix $ Cons 0 . fmap (Stream.map (+1))

fib :: Stream Int
fib = lfix $ Cons 0 . fmap (\s -> Cons 1 ((Stream.zipWith (+) s) <$> (Stream.tail s)))

primes :: Stream Int
primes = lfix $ Cons 2 . fmap (Stream.map (+1) . Stream.scanl1 (*))

toggle :: Stream Bool
toggle = lfix $ Cons True . pure . Cons False

-- aka dragon curve
paperfolds :: Stream Bool
paperfolds = lfix $ interleave toggle

thuemorse :: Stream Bool
thuemorse = lfix $ Cons False . fmap (\tm -> Cons True (go <$> (Stream.tail (go tm))))
  where
  go :: Stream Bool -> Stream Bool
  go = lfix $ \g' s -> case s of
                         Cons False s' -> Cons False (pure (Cons True  (g' <*> s')))
                         Cons True  s' -> Cons True  (pure (Cons False (g' <*> s')))

pascal :: Stream (Stream Int)
pascal = lfix $ Cons (forever 1) . fmap (Stream.map go)
  where
  go :: Stream Int -> Stream Int
  go xs = lfix $ Cons 1 . (((Stream.zipWith (+)) <$> (Stream.tail xs)) <*>)

