{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Chunks.Pipe where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.State.Class
import Data.Pipe
import Data.Bits
import Data.ByteString qualified as BS

takeByteString :: (MonadState m, StateType m ~ BS.ByteString) =>
	Int -> Pipe BS.ByteString () m BS.ByteString
takeByteString n = do
	obs <- get
	let	ln = BS.length obs
	if ln >= n
	then BS.take n obs <$ modify (BS.drop n)
	else do	mbs <- await
		case mbs of
			Just nbs -> obs `BS.append` BS.take (n - ln) nbs
				<$ put (BS.drop (n - ln) nbs)
			Nothing -> pure obs

{-
chunk :: (MonadState m, StateType m ~ BS.ByteString) =>
	(BS.ByteString -> BS.ByteString -> SomeChunk) ->
	Pipe BS.ByteString SomeChunk m ()
chunk f = do
	-}

dataLength :: (MonadState m, StateType m ~ BS.ByteString) =>
	Pipe BS.ByteString () m Int
dataLength = bsToNum32 <$> takeByteString 4

bsToNum32 :: (Bits n, Integral n) => BS.ByteString -> n
bsToNum32 bs
	| BS.length bs == 4 = bigEndian 0 . (fromIntegral <$>) $ BS.unpack bs
	| otherwise = error "bad"

bigEndian :: Bits n => n -> [n] -> n
bigEndian s [] = s
bigEndian s (n : ns) = bigEndian (s `shiftL` 8 .|. n) ns
