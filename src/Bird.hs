{-# LANGUAGE GADTs #-}

module Bird where

import Data.List
import Later

-- Bird's replaceMin algorithm

data Tree a = Leaf a | Br (Tree a) (Tree a)

rmBody :: (Ord a) => Tree a -> Later a -> (Later (Tree a), a)
rmBody (Leaf x) n' = (Leaf <$> n' , x)
rmBody (Br l r) n' =
  let (l' , nl) = rmBody l n'
      (r' , nr) = rmBody r n'
    in
  (Br <$> l' <*> r', min nl nr)

replaceMin :: (Ord a) => Tree a -> Later (Tree a)
replaceMin t = feedback (rmBody t)

-- non-empty list version
-- see also https://www.youtube.com/watch?v=IJhaks0IA2w

{-@ rmlBody :: (Ord a) => {xs : [a] | len xs > 0} -> Later a -> (Later [a], a) @-}
rmlBody :: (Ord a) => [a] -> Later a -> (Later [a], a)
rmlBody [x]    n' = (singleton <$> n' , x)
rmlBody (x:xs) n' =
  let (xs' , n) = rmlBody xs n' in
  ((:) <$> n' <*> xs', min x n)

{-@ replaceMinList :: (Ord a) => {xs : [a] | len xs > 0} -> Later [a] @-}
replaceMinList :: (Ord a) => [a] -> Later [a]
replaceMinList t = feedback (rmlBody t)

