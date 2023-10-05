{-# LANGUAGE GADTs #-}

module Conat where

import Later

data Conat where
  Z :: Conat
  S :: Later Conat -> Conat

infinity :: Conat
infinity = fix S