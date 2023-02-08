{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Misc.Middle.Internal (

	-- * Control

	mapContM, allocaAndPokeArray',

	-- * Values

	pattern NullHandle,

	-- * Bool

	boolToBool32, bool32ToBool,

	-- * Text

	textToCString, cstrToText, textListToCStringArray ) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import qualified Data.Text as Txt
import qualified Data.Text.Foreign as Txt


#include <vulkan/vulkan.h>

textListToCStringArray :: [Txt.Text] -> (Ptr CString -> IO a) -> IO a
textListToCStringArray txts f =
	(textToCString `mapContM` txts) \cstrl ->
	allocaArray (length txts) \pcstra ->
	pokeArray pcstra cstrl >> f pcstra

pattern NullHandle :: Ptr a
pattern NullHandle <- (ptrToWordPtr -> (WordPtr #{const VK_NULL_HANDLE})) where
	NullHandle = wordPtrToPtr $ WordPtr #{const VK_NULL_HANDLE}

textToCString :: Txt.Text -> (CString -> IO a) -> IO a
textToCString t f = Txt.withCStringLen t \(cs, ln) ->
	allocaArray (ln + 1) \cs' -> do
		copyBytes cs' cs ln
		poke (cs' `plusPtr` ln :: Ptr CChar) 0
		f cs'

cstrToText :: CString -> IO Txt.Text
cstrToText cs = Txt.peekCStringLen =<< cstringToCStringLen cs

cstringLength :: CString -> IO Int
cstringLength pc = do
	c <- peek pc
	case c of
		0 -> pure 0
		_ -> (+ 1) <$> cstringLength (pc `plusPtr` 1)

cstringToCStringLen :: CString -> IO CStringLen
cstringToCStringLen cs = (cs ,) <$> cstringLength cs

boolToBool32 :: Bool -> #{type VkBool32}
boolToBool32 False = #{const VK_FALSE}
boolToBool32 True = #{const VK_TRUE}

bool32ToBool :: #{type VkBool32} -> Bool
bool32ToBool #{const VK_FALSE} = False
bool32ToBool #{const VK_TRUE} = True
bool32ToBool _ = error $
	"Application must not pass any other values than " ++
	"VK_TRUE or VK_FALSE into a Gpu.Vulkan implementation " ++
	"where a VkBool32 is expected"

allocaAndPokeArray' :: Storable a => [a] -> ((Int, Ptr a) -> IO b) -> IO b
allocaAndPokeArray' (length &&& id -> (xc, xs)) f
	= allocaArray xc \p -> pokeArray p xs >> f (xc, p)

mapContM :: Monad m => (a -> (b -> m c) -> m c) -> [a] -> ([b] -> m c) -> m c
mapContM f = runContT . mapM (ContT . f)
