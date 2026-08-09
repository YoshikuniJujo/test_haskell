{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.Bech32.Polymod (generate, verify) where

import Control.Monad.Identity
import Control.Monad.State
import Data.Bits
import Data.Bool
import Data.Word.Yj

gen :: [Word30]
gen = [0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3]

generate :: Pop5Bits a => a -> Word30
generate = fst . runState (generateM . StateT $ Identity . pop5Bits)

generateM :: Monad m => m (Maybe Word5) -> m Word30
generateM gt = (<$> stepsM gt 1)
	$ (`xor` 1) . fst . (`steps` [0 :: Word5, 0, 0, 0, 0, 0])

verify :: Pop5Bits a => a -> Word30
verify = fst . runState (verifyM . StateT $ Identity . pop5Bits)

verifyM :: Monad m => m (Maybe Word5) -> m Word30
verifyM = (`stepsM` 1)

steps :: Pop5Bits a => Word30 -> a -> (Word30, a)
steps = runState . stepsM (StateT $ Identity . pop5Bits)

stepsM :: Monad m => m (Maybe Word5) -> Word30 -> m Word30
stepsM gt w30 = maybe (pure w30) (stepsM gt) =<< stepM gt w30

stepM :: Monad m => m (Maybe Word5) -> Word30 -> m (Maybe Word30)
stepM gt = ((uncurry applyGen <$>) <$>) . shift5M gt

shift5M :: Monad m => m (Maybe Word5) -> Word30 -> m (Maybe (Word5, Word30))
shift5M gt w30 = (<$> gt) (
	(fromIntegral $ w30 `shiftR` 25 ,)
		. (w30 `shiftL` 5 .|.) . fromIntegral <$>)

applyGen :: Word5 -> Word30 -> Word30
applyGen w5 w30 = foldr ($) w30
	$ zipWith (\i g -> bool id (`xor` g) (testBit w5 i)) [0 .. 4] gen

class Pop5Bits a where pop5Bits :: a -> (Maybe Word5, a)
class Pop5BitsList a where pop5BitsList :: [a] -> (Maybe Word5, [a])
instance Pop5BitsList a => Pop5Bits [a] where pop5Bits = pop5BitsList

instance Pop5BitsList Word5 where
	pop5BitsList = \case [] -> (Nothing, []); w : ws -> (Just w, ws)
