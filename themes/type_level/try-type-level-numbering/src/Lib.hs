{-# LANGUAGE TypeFamilies, TypeFamilyDependencies, DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import GHC.TypeLits

type family Number t = (r :: Nat) | r -> t

type instance Number () = 5
type instance Number Bool = 15
-- type instance Number Double = 15
