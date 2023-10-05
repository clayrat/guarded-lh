{-# LANGUAGE GADTs #-}

module Later where

-------------------------------------------------------------------------------
-- | Later Library ------------------------------------------------------------
-------------------------------------------------------------------------------

data Later a = Next a

apL :: Later (a -> b) -> Later a -> Later b
apL (Next f) (Next a) = Next (f a)

{-@ lazy fix @-}
fix :: (Later a -> a) -> a
fix f = f (Next (fix f))

-- Combinators

mapL :: (a -> b) -> Later a -> Later b
mapL f = apL (Next f)
