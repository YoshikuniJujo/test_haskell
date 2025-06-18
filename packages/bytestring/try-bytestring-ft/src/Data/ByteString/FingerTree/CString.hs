{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ByteString.FingerTree.CString where

import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.C.String
import Data.ByteString qualified as BS
import Data.ByteString.FingerTree as BSF

pokeByteString :: CStringLen -> BS.ByteString -> IO (CStringLen, BS.ByteString)
pokeByteString (p, n) bs
	| n >= n' = do
		BS.useAsCStringLen bs \(p', _) -> copyBytes p p' n'
		pure ((plusPtr p n', n - n'), "")
	| otherwise = let (t, d) = BS.splitAt n bs in do
		BS.useAsCStringLen t \(p', _) -> copyBytes p p' n
		pure ((plusPtr p n, 0), d)
	where
	n' = BS.length bs

poke :: CStringLen -> BSF.ByteString -> IO (CStringLen, BSF.ByteString)
poke c@(p, n) bsf
	| n >= n' = ((plusPtr p n', n - n'), "") <$ go c bsf
	| otherwise = case BSF.splitAt' n bsf of
		Nothing -> error "never occur"
		Just (t, d) -> ((plusPtr p n, 0), d) <$ go c t
	where
	go csl = \case
		Empty -> pure ()
		bs BSF.:<| bss -> do
			(csl', "") <- pokeByteString csl bs
			go csl' bss
	n' = BSF.length bsf

peek :: CStringLen -> IO BSF.ByteString
peek csl = (BSF.:<| BSF.Empty) <$> BS.packCStringLen csl
