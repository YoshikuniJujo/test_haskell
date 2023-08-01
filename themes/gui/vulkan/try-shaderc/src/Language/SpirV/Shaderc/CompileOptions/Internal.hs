{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Language.SpirV.Shaderc.CompileOptions.Internal where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Monad.Cont
import Data.Bool

import qualified Data.ByteString as BS

import Shaderc.Enum
import Shaderc.Include

import qualified Shaderc.CompileOptions.Core as C

data C ud = C {
	cMacroDefinitions :: [(BS.ByteString, BS.ByteString)],
	cSourceLanguage :: Maybe SourceLanguage,
	cGenerateDebugInfo :: Bool,
	cOptimizationLevel :: Maybe OptimizationLevel,
	cForcedVersionProfile :: Maybe (Version, Profile),
	cIncludeCallbacks :: Maybe (ResolveFn ud, Maybe ud) }

tToCore :: Storable ud => C ud -> ContT r IO C.C
tToCore C {
	cMacroDefinitions = mds,
	cSourceLanguage = lng,
	cGenerateDebugInfo = dbg,
	cOptimizationLevel = optLvl,
	cForcedVersionProfile = fvp,
	cIncludeCallbacks = mcb
	} = do
	ct <- lift C.initialize
	addMacroDefinitions ct mds
	setSourceLanguage ct lng
	setGenerateDebugInfo ct dbg
	setOptimizationLevel ct optLvl
	setForcedVersionProfile ct fvp
	maybe (pure ()) (uncurry $ setIncludeCallbacks ct) mcb
	ContT \f -> f ct <* C.release ct

addMacroDefinitions :: C.C -> [(BS.ByteString, BS.ByteString)] -> ContT r IO ()
addMacroDefinitions opts = mapM_ . uncurry $ addMacroDefinition opts

addMacroDefinition :: C.C -> BS.ByteString -> BS.ByteString -> ContT r IO ()
addMacroDefinition opts nm vl = do
	(cnm, fromIntegral -> cnmln) <- ContT $ BS.useAsCStringLen nm
	(cvl, fromIntegral -> cvlln) <- ContT $ BS.useAsCStringLen vl
	lift $ C.addMacroDefinition opts cnm cnmln cvl cvlln

setSourceLanguage :: C.C -> Maybe SourceLanguage -> ContT r IO ()
setSourceLanguage opts = maybe (pure ()) $ lift . C.setSourceLanguage opts

setGenerateDebugInfo :: C.C -> Bool -> ContT r IO ()
setGenerateDebugInfo opts = lift . bool (pure ()) (C.setGenerateDebugInfo opts)

setOptimizationLevel :: C.C -> Maybe OptimizationLevel -> ContT r IO ()
setOptimizationLevel opts = maybe (pure ()) $ lift . C.setOptimizationLevel opts

setForcedVersionProfile :: C.C -> Maybe (Version, Profile) -> ContT r IO ()
setForcedVersionProfile opts =
	maybe (pure ()) $ lift . uncurry (C.setForcedVersionProfile opts)

setIncludeCallbacks :: (Storable ud, Pokable ud) =>
	C.C -> ResolveFn ud -> Maybe ud -> ContT r IO ()
setIncludeCallbacks opts rfun mud = do
	let	(crfn, crrfn) = resolveFnToCore rfun
	(castPtr -> pud) <- ContT $ withPokedMaybe mud
	lift $ C.setIncludeCallbacks opts crfn crrfn pud
