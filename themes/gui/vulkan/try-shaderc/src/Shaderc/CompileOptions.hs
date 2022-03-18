{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.CompileOptions where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Pointable
import Control.Monad.Cont
import Data.Bool

import qualified Data.ByteString as BS

import Shaderc.Enum
import Shaderc.Include

import qualified Shaderc.CompileOptions.Core as C

data T ud = T {
	tMacroDefinitions :: [(BS.ByteString, BS.ByteString)],
	tSourceLanguage :: Maybe SourceLanguage,
	tGenerateDebugInfo :: Bool,
	tOptimizationLevel :: Maybe OptimizationLevel,
	tForcedVersionProfile :: Maybe (Version, Profile),
	tIncludeCallbacks :: Maybe (ResolveFn ud, Maybe ud) }

tToCore :: Storable ud => T ud -> ContT r IO C.T
tToCore T {
	tMacroDefinitions = mds,
	tSourceLanguage = lng,
	tGenerateDebugInfo = dbg,
	tOptimizationLevel = optLvl,
	tForcedVersionProfile = fvp,
	tIncludeCallbacks = mcb
	} = do
	ct <- lift C.initialize
	addMacroDefinitions ct mds
	setSourceLanguage ct lng
	setGenerateDebugInfo ct dbg
	setOptimizationLevel ct optLvl
	setForcedVersionProfile ct fvp
	maybe (pure ()) (uncurry $ setIncludeCallbacks ct) mcb
	ContT \f -> f ct <* C.release ct

addMacroDefinitions :: C.T -> [(BS.ByteString, BS.ByteString)] -> ContT r IO ()
addMacroDefinitions opts = mapM_ . uncurry $ addMacroDefinition opts

addMacroDefinition :: C.T -> BS.ByteString -> BS.ByteString -> ContT r IO ()
addMacroDefinition opts nm vl = do
	(cnm, fromIntegral -> cnmln) <- ContT $ BS.useAsCStringLen nm
	(cvl, fromIntegral -> cvlln) <- ContT $ BS.useAsCStringLen vl
	lift $ C.addMacroDefinition opts cnm cnmln cvl cvlln

setSourceLanguage :: C.T -> Maybe SourceLanguage -> ContT r IO ()
setSourceLanguage opts = maybe (pure ()) $ lift . C.setSourceLanguage opts

setGenerateDebugInfo :: C.T -> Bool -> ContT r IO ()
setGenerateDebugInfo opts = lift . bool (pure ()) (C.setGenerateDebugInfo opts)

setOptimizationLevel :: C.T -> Maybe OptimizationLevel -> ContT r IO ()
setOptimizationLevel opts = maybe (pure ()) $ lift . C.setOptimizationLevel opts

setForcedVersionProfile :: C.T -> Maybe (Version, Profile) -> ContT r IO ()
setForcedVersionProfile opts =
	maybe (pure ()) $ lift . uncurry (C.setForcedVersionProfile opts)

setIncludeCallbacks :: (Storable ud, Pointable ud) =>
	C.T -> ResolveFn ud -> Maybe ud -> ContT r IO ()
setIncludeCallbacks opts rfun mud = do
	let	(crfn, crrfn) = resolveFnToCore rfun
	(castPtr -> pud) <- maybeToPointer mud
	lift $ C.setIncludeCallbacks opts crfn crrfn pud
