{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.Bech32 (

	B(..), encode, decode,

	switch, switchM, getData

	) where

import Control.Arrow
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Except
import Data.Bits
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Bool
import Data.Char
import Data.ByteString qualified as BS
import Data.Text qualified as T

import Codec.Bech32.Polymod qualified as Polymod
import Data.Word.Yj
import Tools

data B = B { humanReadPart :: String, dataPart :: BS.ByteString }
	deriving (Show, Eq)

encode :: B -> T.Text
encode B { humanReadPart = hp, dataPart = dp } =
	T.pack $ hp ++ "1" ++ ((dict !!) . fromIntegral <$> w5s <> cs)
	where
	cs = word30ToWord5List . Polymod.generate $ hrpToW5s hp ++ w5s
	w5s = word8sToWord5s $ BS.unpack dp

decode :: T.Text -> Either String B
decode = go <=< sep . T.unpack
	where
	go (h, d) = idx `mapM` d >>= \d' -> bool
		(throwError "Bech32: checksum verification failed")
		(B h . BS.pack <$> word5sToWord8s (takeR 6 d'))
		(Polymod.verify $ hrpToW5s h ++ d')
	idx = maybe (throwError bc) (pure . fromIntegral) . (`L.elemIndex` dict)
	sep = (const ns +++ (NE.init `first`)) . spanR (/= '1')
	bc = "bad character"; ns = "Bech32: no separator '1'"

hrpToW5s :: String -> [Word5]
hrpToW5s ((ord <$>) -> hrp) =
	fromIntegral <$> ((`shiftR` 5) <$> hrp) ++ 0 : ((.&. 0x1f) <$> hrp)

dict :: [Char]
dict = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

switch :: [(String -> Bool, PrcDp a)] -> PrcDp a -> B -> a
switch cs d = runIdentity . switchM (((pure .) *** (pure .)) <$> cs) (pure . d)

type PrcDp a = BS.ByteString -> a

switchM :: Monad m => [(String -> m Bool, PrcDpM m a)] -> PrcDpM m a -> B -> m a
switchM cs df B { humanReadPart = h, dataPart = d } = go cs
	where go = \case [] -> df d; (p, f) : pfs -> bool (go pfs) (f d) =<< p h

type PrcDpM m a = BS.ByteString -> m a

getData :: String -> B -> Either String BS.ByteString
getData h0 = switch [((== h0), pure)] (const $ throwError msg)
	where msg = "HRP should be " ++ show h0
