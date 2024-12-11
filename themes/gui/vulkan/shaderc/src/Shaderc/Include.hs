{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.Include where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Monad.Trans
import Control.Monad.Cont

import qualified Data.ByteString as BS

import qualified Shaderc.Include.Core as C

data Result ud = Result {
	resultSourceName :: BS.ByteString,
	resultContent :: BS.ByteString,
	resultUserData :: Maybe ud }
	deriving Show

resultToCore :: Pokable ud => Result ud -> ContT r IO C.Result
resultToCore Result {
	resultSourceName = src,
	resultContent = cnt,
	resultUserData = ud } = do
	(csrc, fromIntegral -> csrcln) <- ContT $ BS.useAsCStringLen src
	(ccnt, fromIntegral -> ccntln) <- ContT $ BS.useAsCStringLen cnt
	(castPtr -> pud) <- ContT $ withPokedMaybe ud
	pure $ C.Result {
		C.resultSourceName = csrc,
		C.resultSourceNameLength = csrcln,
		C.resultContent = ccnt,
		C.resultContentLength = ccntln,
		C.resultUserData = pud }

type ResolveFn ud =
	Maybe ud -> BS.ByteString -> C.Type -> BS.ByteString -> Int ->
	IO (Result ud)

resolveFnToCore :: Storable ud => ResolveFn ud -> (C.ResolveFn, C.ResultReleaseFn)
resolveFnToCore f = (resolveFnToResolveFnCore f, resultReleaseFn)

resultReleaseFn :: C.ResultReleaseFn
resultReleaseFn pud prslt = free pud >> free prslt

resolveFnToResolveFnCore :: Storable ud => ResolveFn ud -> C.ResolveFn
resolveFnToResolveFnCore f pud crqtd tp crqtng dpt = do
	ud <- case pud of
		NullPtr -> pure Nothing
		_ -> Just <$> peek (castPtr pud)
	rqtd <- BS.packCString crqtd
	rqtng <- BS.packCString crqtng
	prslt <- malloc
	rslt <- f ud rqtd tp rqtng $ fromIntegral dpt
	($ pure) . runContT
		$ lift . poke prslt =<< resultToCore rslt
	pure prslt
