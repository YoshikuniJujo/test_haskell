{-# LANGUAGE ScopedTypeVariables, TypeApplications, DataKinds, KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module StorableByteString where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.C.Types
import GHC.TypeLits
import Control.Monad
import Data.Proxy
import Data.String

import qualified Data.ByteString as BS

newtype StorableByteString (n :: Nat) = StorableByteString BS.ByteString
	deriving Show

instance KnownNat n => Storable (StorableByteString n) where
	sizeOf _ = fromInteger $ natVal @n Proxy
	alignment _ = alignment @CChar undefined
	peek p = StorableByteString
		<$> BS.packCStringLen (castPtr p, fromInteger $ natVal @n Proxy)
	poke p (StorableByteString bs) = BS.useAsCStringLen bs $ \(cs, l) -> do
		copyBytes (castPtr p) cs $ min l ln
		when (l < ln) $ fillBytes (p `plusPtr` l) 0 (ln - l)
		where ln = fromInteger $ natVal @n Proxy

instance IsString (StorableByteString n) where
	fromString = StorableByteString . fromString
