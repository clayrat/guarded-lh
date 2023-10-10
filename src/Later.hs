{-# LANGUAGE GADTs #-}

module Later (Later, lfix, feedback) where

-------------------------------------------------------------------------------
-- | Later Library ------------------------------------------------------------
-------------------------------------------------------------------------------

data Later a = Next a deriving Show

instance Functor Later where
  fmap f (Next x) = Next (f x)

instance Applicative Later where
  pure = Next
  Next f <*> Next x = Next (f x)

{-@ lazy dfix @-}
dfix :: (Later a -> a) -> Later a
dfix f = Next (f (dfix f))

{-@ lazy lfix @-}
lfix :: (Later a -> a) -> a
lfix f = f (Next (lfix f))
--lfix f = f (dfix f)

-- combinators

feedback :: (Later a -> (b , a)) -> b
feedback f = fst (lfix (f . fmap snd))
