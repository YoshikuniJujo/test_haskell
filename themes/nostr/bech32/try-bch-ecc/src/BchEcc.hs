{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BchEcc where

import Control.Monad.Identity
import Control.Monad.State
import Data.Bits

import MyWords

gen :: [Word30]
gen = [	0x3b6a57b2,
	0x26508e6d,
	0x1ea119fa,
	0x3d4233dd,
	0x2a1462b3 ]

applyGen :: Word5 -> Word30 -> Word30
applyGen w5 w30 = let
	gen' = zipWith (\i g -> if testBit w5 i then (`xor` g) else id) [0 .. 4] gen in
	foldr ($) w30 gen'

shift5M :: Monad m => m (Maybe Word5) -> Word30 -> m (Maybe (Word5, Word30))
shift5M gt w30 = do
	mdt <- gt
	case mdt of
		Nothing -> pure Nothing
		Just dt -> let	bs = w30 `shiftR` 25
				w30' = w30 `shiftL` 5 .|. fromIntegral dt in
			pure $ Just (fromIntegral bs, w30')

class Pop5Bits a where pop5Bits :: a -> (Maybe Word5, a)

shift5 :: Pop5Bits a => Word30 -> a -> (Maybe (Word5, Word30), a)
shift5 w30 ws = shift5M gt w30 `runState` ws
	where
	gt = StateT $ Identity . pop5Bits

stepM :: Monad m => m (Maybe Word5) -> Word30 -> m (Maybe Word30)
stepM gt w30 = do
	p <- shift5M gt w30
	pure $ uncurry applyGen <$> p

step :: Pop5Bits a => Word30 -> a -> (Maybe Word30, a)
step w30 ws = stepM gt w30 `runState` ws
	where
	gt = StateT $ Identity . pop5Bits

stepsM :: Monad m => m (Maybe Word5) -> Word30 -> m Word30
stepsM gt w30 = stepM gt w30 >>= \case
	Nothing -> pure w30
	Just w30' -> stepsM gt w30'

steps :: Pop5Bits a => Word30 -> a -> (Word30, a)
steps w30 ws = stepsM gt w30 `runState` ws
	where
	gt = StateT $ Identity . pop5Bits

newtype Word5List = Word5List [Word5] deriving Show

instance Pop5Bits Word5List where
	pop5Bits (Word5List []) = (Nothing, Word5List [])
	pop5Bits (Word5List (w : ws)) = (Just w, Word5List ws)

polymodM :: Monad m => m (Maybe Word5) -> m Word30
polymodM gt = do
	w30 <- stepsM gt 1
	pure $ fst (steps w30 $ Word5List [0, 0, 0, 0, 0, 0]) `xor` 1

polymod :: Pop5Bits a => a -> Word30
polymod ws = fst $ polymodM gt `runState` ws
	where
	gt = StateT $ Identity . pop5Bits

polymodL :: [Word5] -> Word30
polymodL = polymod . Word5List
