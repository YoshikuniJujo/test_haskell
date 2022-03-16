{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.CompileOptions where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Pointable
import Control.Monad.Cont

import Shaderc.Enum
import Shaderc.Include

import qualified Shaderc.CompileOptions.Core as C

data T ud = T {
	tMacroDefinition :: [(String, String)],
	tSourceLanguage :: Maybe SourceLanguage,
	tGenerateDebugInfo :: Bool,
	tOptimizationLevel :: Maybe OptimizationLevel,
	tForcedVersionProfile :: Maybe (Version, Profile),
	tIncludeCallbacks :: Maybe (ResolveFn ud, Maybe ud) }

setIncludeCallbacks :: (Storable ud, Pointable ud) =>
	C.T -> ResolveFn ud -> Maybe ud -> ContT r IO ()
setIncludeCallbacks opts rfun mud = do
	let	(crfn, crrfn) = resolveFnToCore rfun
	(castPtr -> pud) <- maybeToPointer mud
	lift $ C.setIncludeCallbacks opts crfn crrfn pud
