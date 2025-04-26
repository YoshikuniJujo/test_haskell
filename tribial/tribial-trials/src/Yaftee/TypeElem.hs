{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Yaftee.TypeElem where

newtype P t ts = P { unP :: Word }

class Member (t :: k) (ts :: [k]) where elemNo :: P t ts

instance Member t (t ': ts) where elemNo = P 0

instance {-# OVERLAPPABLE #-} Member t ts => Member t (_t' ': ts) where
	elemNo = P $ 1 + unP (elemNo :: P t ts)

class Base (t :: k) (ts :: [k]) where elemNoBase :: P t ts

instance Base t '[t] where elemNoBase = P 0

instance {-# OVERLAPPABLE #-} Base t ts => Base t (_t' ': ts) where
	elemNoBase = P $ 1 + unP (elemNoBase :: P t ts)
