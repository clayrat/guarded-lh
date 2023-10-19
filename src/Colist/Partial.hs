{-# LANGUAGE GADTs #-}

module Colist.Partial where

import Later
import Colist
import Conat
import Part

foldl :: (b -> a -> b) -> b -> Colist a -> Part b
foldl f = lfix $ \f' z xs -> case xs of
                               Nil        -> Now z
                               Cons x xs' -> Delay (f' <*> pure (f z x) <*> xs')
