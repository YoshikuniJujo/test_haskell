{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.Storable.PeekPoke (

	-- * Sizable, Peek and Poke

	Sizable(..), Peek(..), peekMaybe, Poke(..),

	-- * Peekable, WithPoked and Pokable

	Peekable, peekArray',
	WithPoked(..), PtrS, ptrS, withPtrS, withPokedMaybe',
	Pokable, withPoked, withPokedMaybe,

	-- * Storable' and NullPtr

	Storable', pattern NullPtr ) where

import Foreign.Storable.PeekPoke.Internal
