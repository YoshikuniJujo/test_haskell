{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeLevel.List where

import Data.Kind

type family MapFst (zl :: [(Type, Type)]) where
	MapFst '[] = '[]
	MapFst ('(x, y) ': xys) = x ': MapFst xys

type family Zip (xs :: [Type]) (ys :: [Type]) where
	Zip '[] ys = '[]
	Zip xs '[] = '[]
	Zip (x ': xs) (y ': ys) = '(x, y) ': Zip xs ys

type family MapFst' (zl :: [Type]) where
	MapFst' '[] = '[]
	MapFst' ((x, y) : xys) = x ': MapFst' xys
