{-# LANGUAGE GADTs #-}

module Conat.Partial where

import Later
import Conat
import Part

sub :: Conat -> Conat -> Part Conat
sub = lfix $ \s' x y -> case (x, y) of
                          (Z   , _   ) -> Now Z
                          (S x', Z   ) -> Now (S x')
                          (S x', S y') -> Delay (s' <*> x' <*> y')
