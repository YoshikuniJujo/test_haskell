{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Type.SetApp.Internal where

import Data.Kind
import Data.Type.Set.Internal

data SetApp a = SetApp (Type -> Type) (Set a)

infixl 4 :$:

type f :$: ts = 'SetApp f (f `Map` ts)
