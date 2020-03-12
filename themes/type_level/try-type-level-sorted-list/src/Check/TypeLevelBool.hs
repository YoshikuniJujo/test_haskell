{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Check.TypeLevelBool where

class If (b :: Bool) a where
	value :: a

instance If 'False Bool where value = False
instance If 'True Bool where value = True

instance If 'False Integer where value = 0
instance If 'True Integer where value = 1
