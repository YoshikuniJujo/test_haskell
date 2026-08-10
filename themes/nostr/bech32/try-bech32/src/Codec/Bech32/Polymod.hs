{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.Bech32.Polymod (

	generate, generateM, verify, verifyM,

	Pop5Bits(..), Pop5BitsList(..)

	) where

import Control.Monad.State
import Data.Monoid
import Data.Bits
import Data.Bool
import Data.Word.Yj

generate :: Pop5Bits a => a -> Word30
generate = fromM generateM

generateM :: Monad m => m (Maybe Word5) -> m Word30
generateM = ((`xor` 1) . padding <$>) . (`polymod` 1)

padding :: Word30 -> Word30
padding = fst . (`runState` pd) . polymod (state pop5Bits)
	where pd = replicate 6 0 :: [Word5]

verify :: Pop5Bits a => a -> Bool
verify = fromM verifyM

verifyM :: Monad m => m (Maybe Word5) -> m Bool
verifyM = ((== 1) <$>) . (`polymod` 1)

fromM :: Pop5Bits s => (State s (Maybe Word5) -> State s a) -> s -> a
fromM = (fst .) . runState . ($ state pop5Bits)

polymod :: Monad m => m (Maybe Word5) -> Word30 -> m Word30
polymod gw cs = maybe (pure cs) (polymod gw) =<< step gw cs

step :: Monad m => m (Maybe Word5) -> Word30 -> m (Maybe Word30)
step gw = ((uncurry applyGen <$>) <$>) . shift5 gw

shift5 :: Monad m => m (Maybe Word5) -> Word30 -> m (Maybe (Word5, Word30))
shift5 gw cs = ((h5 ,) . (l25 .|.) . fromIntegral <$>) <$> gw
	where h5 = fromIntegral $ cs `shiftR` 25; l25 = cs `shiftL` 5

applyGen :: Word5 -> Word30 -> Word30
applyGen w = appEndo . foldMap Endo
	$ zipWith (\i g -> bool id (`xor` g) (testBit w i)) [0 .. 4] gen

gen :: [Word30]
gen = [0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3]

class Pop5Bits a where pop5Bits :: a -> (Maybe Word5, a)
class Pop5BitsList a where pop5BitsList :: [a] -> (Maybe Word5, [a])
instance Pop5BitsList a => Pop5Bits [a] where pop5Bits = pop5BitsList

instance Pop5BitsList Word5 where
	pop5BitsList = \case [] -> (Nothing, []); w : ws -> (Just w, ws)
