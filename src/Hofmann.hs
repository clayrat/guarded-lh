{-# LANGUAGE GADTs #-}

module Hofmann where

import Later
import Colist

data Tree a = Leaf a | Node (Tree a) a (Tree a)

{-@ measure treeSize @-}
{-@ treeSize :: Tree a -> Nat @-}
treeSize :: Tree a -> Int
treeSize (Leaf _)     = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r

data Rou a where
  OverR :: Rou a
  NextR :: ((Later (Rou a) -> Later (Colist a)) -> Colist a) -> Rou a

unfold :: Rou a -> (Later (Rou a) -> Later (Colist a)) -> Later (Colist a)
unfold  OverR    f = f (Next OverR)
unfold (NextR g) f = Next (g f)

{-@ br :: t:Tree a -> Rou a -> Rou a / [treeSize t] @-}
br :: Tree a -> Rou a -> Rou a
br (Leaf a)     k = NextR (\f -> a `Cons` unfold k f)
br (Node l a r) k = NextR (\f -> a `Cons` unfold k (f . mapL (br l . br r)))

ex :: Rou a -> Colist a
ex = fix $
     \f x -> case x of
               OverR -> Nil
               NextR g -> g (apL f)

bft :: Tree a -> Colist a
bft t = ex $ br t OverR
