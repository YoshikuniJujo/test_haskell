{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CircuitTypes where

import Prelude
import qualified Prelude as P

import Control.Monad.State
import Data.Bits ((.&.), (.|.))
import Data.Array
import Data.Map.Strict
import Data.Word

import qualified Data.Bits as B
import qualified Data.List as L
import qualified Data.IntMap.Strict as IM
import qualified Data.Array as A

import Tools

newtype IWire = IWire Word32 deriving (Show, Eq, Ord)
newtype OWire = OWire Word32 deriving (Show, Eq, Ord)

newtype Bits = Bits Word64 deriving (Show, Eq)

bitsToWord :: Bits -> Word64
bitsToWord (Bits w) = w

wordToBits :: Word64 -> Bits
wordToBits = Bits

andBits :: BitLen -> BitPosOut ->
	(Bits, BitPosIn) -> (Bits, BitPosIn) -> Bits -> Bits
andBits ln po (Bits i1, pi1) (Bits i2, pi2) (Bits w) = Bits $ clr .|. i1' .&. i2'
	where
	clr = w .&. windowBits ln po
	i1' = (i1 `B.shift` (fromIntegral po - fromIntegral pi1)) .&. maskBits ln po
	i2' = (i2 `B.shift` (fromIntegral po - fromIntegral pi2)) .&. maskBits ln po

orBits :: BitLen -> BitPosOut ->
	(Bits, BitPosIn) -> (Bits, BitPosIn) -> Bits -> Bits
orBits ln po (Bits i1, pi1) (Bits i2, pi2) (Bits w) = Bits $ clr .|. (i1' .|. i2')
	where
	clr = w .&. windowBits ln po
	i1' = (i1 `B.shift` (fromIntegral po - fromIntegral pi1)) .&. maskBits ln po
	i2' = (i2 `B.shift` (fromIntegral po - fromIntegral pi2)) .&. maskBits ln po

notBits :: BitLen -> BitPosOut -> (Bits, BitPosIn) -> Bits -> Bits
notBits ln po (Bits i, pin) (Bits w) = Bits $ clr .|. (B.complement i' .&. maskBits ln po)
	where
	clr = w .&. windowBits ln po
	i' = (i `B.shift` (fromIntegral po - fromIntegral pin))

idBits :: BitLen -> BitPosOut -> (Bits, BitPosIn) -> Bits -> Bits
idBits ln po (Bits i, pin) (Bits w) = Bits $ clr .|. (i' .&. maskBits ln po)
	where
	clr = w .&. windowBits ln po
	i' = (i `B.shift` (fromIntegral po - fromIntegral pin))

constBits :: BitLen -> BitPosOut-> (Bits, BitPosIn) -> Bits -> Bits
constBits ln po (Bits i, pin) (Bits w) = Bits $ clr .|. i'
	where
	clr = w .&. windowBits ln po
	i' = (i `B.shift` (fromIntegral po - fromIntegral pin)) .&.
		maskBits ln po

-- maskBits, windowBits :: B.Bits w => BitLen -> BitPosOut -> w
maskBits, maskBits', windowBits :: BitLen -> BitPosOut -> Word64
windowBits ln ps = B.complement $ maskBits ln ps
maskBits' ln ps =
	L.foldl' B.setBit B.zeroBits $ fromIntegral <$> [ps .. ps + ln - 1]

-- maskBits ln ps = (maskBitsList !! fromIntegral ln) !! fromIntegral ps -- maskBits'
maskBits ln ps = maskBitsList A.! (fromIntegral ln * 64 + fromIntegral ps)

maskBitsList :: Array Int Word64
-- maskBitsList = P.map (\ln -> P.map (maskBits' ln) [0 .. 63]) [0 .. 64]
maskBitsList = listArray (0, 4159) $  P.concatMap (\ln -> P.map (maskBits' ln) [0 .. 63]) [0 .. 64]

type FromOWire = ((BitLen, BitPosOut), (BitLen, BitPosIn))

fromOWire :: FromOWire -> Bits -> Bits -> Bits
fromOWire ((blo, bpo_), (bli, bpi_)) (Bits bo) (Bits bi)
	| blo == bli = Bits $ bo'' .|. bi'
	| otherwise = Bits $ bo' .|. bi'
	where
	bo'' = (bo `B.shiftR` bpo) `B.shiftL` bpi .&. maskBits bli bpi_
	bo' = (bo `B.shiftR` bpo) `cycleBits`
		blo `B.shiftL` bpi .&. maskBits bli bpi_
	bi' = bi .&. windowBits bli bpi_
	[bpo, bpi] = fromIntegral <$> [bpo_, bpi_]

-- cycleBits :: forall n . B.Bits n => n -> Word8 -> n
cycleBits :: Word64 -> Word8 -> Word64
cycleBits _ 0 = error "cycleBits n c: c should not be 0"
cycleBits n c = cb $ 64 `div` c + signum (64 `mod` c)
	where
--	cb :: Word8 -> n
	cb i | i < 1 = B.zeroBits
	cb i = cb (i - 1) `B.shiftL` fromIntegral c .|. n .&. maskBits c 0

data Circuit = Circuit {
	cctGate :: Map OWire [BasicGate],
	cctWireConn :: Map IWire [(OWire, FromOWire)],
	cctWireStt :: Map IWire [Bits] } deriving Show

type CircuitBuilder = State CBState

type BitLen = Word8
type BitPosIn = Word8
type BitPosOut = Word8

data CBState = CBState {
	cbsWireNum :: Word32,
	cbsGate :: Map OWire [BasicGate],
	cbsWireConn :: Map IWire [(OWire, FromOWire)],
	cbsDelay :: Map IWire Word8
	} deriving Show

initCBState :: CBState
initCBState = CBState {
	cbsWireNum = 0, cbsGate = empty, cbsWireConn = empty, cbsDelay = empty }

data BasicGate
	= ConstGate BitLen BitPosOut (Bits, BitPosIn)
	| AndGate BitLen BitPosOut (IWire, BitPosIn) (IWire, BitPosIn)
	| OrGate BitLen BitPosOut (IWire, BitPosIn) (IWire, BitPosIn)
	| NotGate BitLen BitPosOut (IWire, BitPosIn)
	| IdGate BitLen BitPosOut (IWire, BitPosIn)
	| TriStateSelect IWire (IM.IntMap IWire)
	deriving Show

gateWires :: BasicGate -> [IWire]
gateWires (ConstGate _ _ _) = []
gateWires (AndGate _ _ (a, _) (b, _)) = [a, b]
gateWires (OrGate _ _ (a, _) (b, _)) = [a, b]
gateWires (NotGate _ _ (i, _)) = [i]
gateWires (IdGate _ _ (i, _)) = [i]
gateWires (TriStateSelect i is) = i : (snd <$> IM.toList is)

makeIWire :: CircuitBuilder IWire
makeIWire = IWire <$> getModify cbsWireNum sccWireNum

makeOWire :: CircuitBuilder OWire
makeOWire = OWire <$> getModify cbsWireNum sccWireNum

sccWireNum :: CBState -> CBState
sccWireNum cbs = cbs { cbsWireNum = cbsWireNum cbs + 1 }
