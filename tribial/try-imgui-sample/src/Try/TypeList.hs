{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.TypeList where

import GHC.TypeLits
import Data.TypeLevel.Tuple.Uncurry
import Data.HeteroParList (pattern (:**))
import Data.HeteroParList qualified as HPList

newtype Foo (a :: Natural) = Foo Word deriving Show
newtype Bar (b :: Symbol) = Bar String deriving Show

data FooBar a b = FooBar { foo :: Foo a, bar :: Bar b } deriving Show

class FooBars as bs where
	type ABs as bs :: [(Natural, Symbol)]
	fooBars :: HPList.PL Foo as -> HPList.PL Bar bs ->
		HPList.PL (U2 FooBar) (ABs as bs)

instance FooBars '[] '[] where
	type ABs '[] '[] = '[]
	fooBars HPList.Nil HPList.Nil = HPList.Nil

instance FooBars as bs =>
	FooBars (a ': as) (b ': bs) where
	type ABs (a ': as) (b ': bs) = '(a, b) ': ABs as bs
	fooBars (f :** fs) (b :** bs) = U2 (FooBar f b) :** fooBars fs bs

hello :: HPList.PL (U2 FooBar) '[ '(a1, b1), '(a2, b2)]
hello = fooBars
	(Foo 8 :** Foo 16 :** HPList.Nil)
	(Bar "Hello" :** Bar "World" :** HPList.Nil)
