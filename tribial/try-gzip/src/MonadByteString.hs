{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadByteString where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Base
import Control.Monad.State
import Control.Monad.Except
import Data.Bits
import Data.Word
import Data.ByteString qualified as BS

import Control.MonadClasses.State qualified as MC
import Control.MonadClasses.Except qualified as MC

pop :: (MC.MonadError String m, MC.MonadState BS.ByteString m) => m Word8
pop = do
	bs <- MC.get
	case BS.uncons bs of
		Nothing -> MC.throwError @String "empty"
		Just (b, bs') -> b <$ MC.put bs'

takeBytes :: (MC.MonadState BS.ByteString m, MC.MonadError String m) =>
	Int -> m BS.ByteString
takeBytes n = do
	bs <- MC.get
	when (BS.length bs < n) $ MC.throwError @String "not enough"
	BS.take n bs <$ MC.put (BS.drop n bs)

takeWord32 :: (MC.MonadState BS.ByteString m, MC.MonadError String m) =>
	m Word32
takeWord32 = bsToNum <$> takeBytes 4

takeString :: (MC.MonadState BS.ByteString m, MC.MonadError String m) =>
	m BS.ByteString
takeString = do
	bs <- MC.get
	let	(str, bs') = BS.span (/= 0) bs
	str <$ MC.put bs' <* pop

print' :: (MonadBase IO m, Show a) => a -> m ()
print' = liftBase . print

putStrLn' :: MonadBase IO m => String -> m ()
putStrLn' = liftBase . putStrLn

bsToNum :: (Bits n, Integral n) => BS.ByteString -> n
bsToNum = foldr (\b s -> fromIntegral b .|. s `shiftL` 8) 0 . BS.unpack

bits :: Word8 -> [Bool]
bits b = (b `testBit`) <$> [0 .. 7]
