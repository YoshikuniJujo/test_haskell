{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Try.TH

mkXy "xy"
mkXy "x"
mkXy "y"

mkXy "xyz"

data Const a b = Const { runConst :: a } deriving Show

instance Functor (Const a) where _ `fmap` Const a = Const a

data Identity a = Identity { runIdentity :: a } deriving Show

instance Functor Identity where f `fmap` Identity x = Identity $ f x
