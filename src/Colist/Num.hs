{-# LANGUAGE GADTs #-}

module Colist.Num where

import Later
import Colist
import Conat

size :: Colist a -> Conat
size = lfix $ \s' xs -> case xs of
                          Nil        -> Z
                          Cons _ xs' -> S (s' <*> xs')