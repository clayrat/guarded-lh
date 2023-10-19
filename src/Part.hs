{-# LANGUAGE GADTs #-}

module Part where

import Later

-- partiality monad

data Part a where
  Now   :: a -> Part a
  Delay :: Later (Part a) -> Part a

never :: Part a
never = lfix Delay

stall :: Part a -> Part a
stall = Delay . pure

{-@ delayBy :: Nat -> a -> Part a @-}
delayBy :: Int -> a -> Part a
delayBy n a | n == 0    = Now a
            | otherwise = stall (delayBy (n-1) a)

instance Functor Part where
  fmap f (Now a) = Now (f a)
  fmap f (Delay p') = Delay (fmap (fmap f) p')

instance Applicative Part where
  pure = Now
  (Now f)     <*> (Now a)     = Now (f a)
  (Now f)     <*> (Delay pa') = Delay (fmap ((Now f) <*>) pa')
  (Delay pf') <*> (Now a)     = Delay (fmap (<*> (Now a)) pf')
  (Delay pf') <*> (Delay pa') = Delay (fmap (<*>) pf' <*> pa')

instance Monad Part where
  (Now x)    >>= f = f x
  (Delay p') >>= f = Delay (fmap (>>= f) p')

unfold :: (b -> Either a b) -> b -> Part a
unfold f = lfix $ \u' b -> case f b of
                             Left a -> Now a
                             Right b1 -> Delay (u' <*> pure b1)

{-@ tryMore :: (Nat -> Maybe a) -> Part a @-}
tryMore :: (Int -> Maybe a) -> Part a
tryMore f = unfold try 0
  where
  try n = case f n of
            Just x  -> Left x
            Nothing -> Right (n+1)

{-@ minimize :: (Nat -> Bool) -> Part Nat @-}
minimize :: (Int -> Bool) -> Part Int
minimize p = tryMore $ \n -> if p n then Just n else Nothing

race :: Part a -> Part a -> Part a
race = lfix $ \r' p1 p2 -> case (p1, p2) of
                             (Now a1   , _        ) -> Now a1
                             (Delay _  , Now a2   ) -> Now a2
                             (Delay p1', Delay p2') -> Delay (r' <*> p1' <*> p2')

traverseP :: (a -> Later b) -> Part a -> Later (Part b)
traverseP f = lfix $ \t' p -> case p of
                                Now a    -> fmap Now (f a)
                                Delay p' -> fmap Delay (t' <*> p')