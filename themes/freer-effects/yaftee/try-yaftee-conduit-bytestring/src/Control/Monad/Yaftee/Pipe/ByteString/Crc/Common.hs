{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.ByteString.Crc.Common (

	-- * RUN STATE

	runCrc32, resetCrc32, compCrc32,


	-- * TYPE

	Crc32(..), crc32Step, initialCrc32, complementCrc32

	) where

import Control.Arrow
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.HigherFunctor qualified as HFunctor
import Data.Bits
import Data.Bits.ToolsYj
import Data.Array
import Data.Bool
import Data.Word

runCrc32 :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (State.Named nm Crc32 ': es) i o r -> Eff.E es i o (r, Crc32)
runCrc32 = (`State.runN` Crc32 0)

resetCrc32 :: forall nm -> U.Member (State.Named nm Crc32) es =>
	Eff.E es i o ()
resetCrc32 nm = State.putN nm $ Crc32 0xffffffff

compCrc32 :: forall nm -> U.Member (State.Named nm Crc32) es => Eff.E es i o ()
compCrc32 nm = State.modifyN nm \(Crc32 c) -> Crc32 $ complement c

newtype Crc32 = Crc32 { unCrc32 :: Word32 } deriving (Show, Eq)

crc1 :: Word32 -> Word32
crc1 = uncurry (bool id (`xor` 0xedb88320)) . popBit

crc8 :: Word8 -> Word32
crc8 n = iterate crc1 (fromIntegral n) !! 8

table :: Array Word8 Word32
table = listArray (0, 255) $ map crc8 [0 .. 255]

popByte :: (Integral a, Bits a) => a -> (Word8, a)
popByte n = (fromIntegral n, n `shiftR` 8)

crc32Step :: Word32 -> Word8 -> Word32
crc32Step n b = uncurry xor . (first $ (table !) . (`xor` b)) $ popByte n

initialCrc32 :: Crc32
initialCrc32 = Crc32 0xffffffff

complementCrc32 :: Crc32 -> Crc32
complementCrc32 = Crc32 . complement . unCrc32
