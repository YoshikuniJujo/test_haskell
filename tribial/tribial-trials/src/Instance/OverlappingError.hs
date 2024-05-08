{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Instance.OverlappingError where

import Data.Kind

class ElemIndex x (xs :: [Type]) where elemIndex :: Int

instance ElemIndex x (x ': xs) where elemIndex = 0

instance {-# OVERLAPPABLE #-}
	ElemIndex x xs => ElemIndex x (y ': xs) where 
	elemIndex = 1 + elemIndex @x @xs

sample1 :: Int
sample1 = elemIndex @Bool @'[Int, Integer, Bool, Char]

elemIndex' :: forall x xs . ElemIndex x xs => Int
elemIndex' = elemIndex @x @xs
