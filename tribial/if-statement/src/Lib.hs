{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib (ifs, ifs') where

class Default a where
	def :: a

instance {-# OVERLAPPABLE #-} Num a => Default a where
	def = 0

instance Default () where
	def = ()

instance {-# OVERLAPPABLE #-} (Applicative f, Default a) => Default (f a) where
	def = pure def

instance Default (Maybe a) where
	def = Nothing

ifs :: Default a => Bool -> a -> a
ifs True x = x
ifs False _ = def

ifs' :: Monoid a => Bool -> a -> a
ifs' True x = x
ifs' False _ = mempty
