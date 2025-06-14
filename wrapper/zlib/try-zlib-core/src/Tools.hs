{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools (bsToPtrW8, ptrW8ToBs) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Data.Word
import Data.ByteString qualified as BS

bsToPtrW8 :: BS.ByteString -> (Ptr Word8 -> Int -> IO a) -> IO a
bsToPtrW8 bs a = do
	let	ws = BS.unpack bs
		ln = BS.length bs
	allocaArray ln \p -> do
		pokeArray p ws
		a p ln

ptrW8ToBs :: Ptr Word8 -> Int -> IO BS.ByteString
ptrW8ToBs p ln = BS.pack <$> peekArray ln p
