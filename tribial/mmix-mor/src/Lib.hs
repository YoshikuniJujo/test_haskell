{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Control.Arrow
import Data.Foldable
import Data.Bits hiding (bit)
import Data.Word

data Bit = O | I deriving Show

bit :: a -> a -> Bit -> a
bit o _ O = o
bit _ i I = i

boolToBit :: Bool -> Bit
boolToBit False = O
boolToBit True = I

infixr 7 .&
infixr 6 .^
infixr 5 .|

(.&), (.|), (.^) :: Bit -> Bit -> Bit
I .& I = I
_ .& _ = O
O .| O = O
_ .| _ = I
O .^ O = O
I .^ I = O
_ .^ _ = I

get :: Int -> Int -> Word64 -> Bit
get i j = boolToBit . flip testBit (i * 8 + j)

set :: Int -> Int -> Word64 -> Bit -> Word64
set i j w b = bit clearBit setBit b w (i * 8 + j)

calc, calc' :: Word64 -> Word64 -> Int -> Int -> Bit
calc y z i j = foldr (\k -> (get k j y .& get i k z .|)) O [0 .. 7]
calc' y z i j = foldr (\k -> (get k j y .& get i k z .^)) O [0 .. 7]

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where f `fmap` State k = State \s -> first f $ k s

instance Applicative (State s) where
	pure x = State \s -> (x, s)
	State kf <*> mx =
		State \s -> let (f, s') = kf s in runState (f <$> mx) s'

instance Monad (State s) where
	State k >>= f = State \s -> let (x, s') = k s in runState (f x) s'

modify :: (s -> s) -> State s ()
modify f = State \s -> ((), f s)

mor, mxor :: Word64 -> Word64 -> Word64
mor y z = snd . (`runState` zeroBits) $ forM_ [0 .. 7] \i ->
	forM_ [0 .. 7] \j -> modify (flip (set i j) (calc y z i j))

mxor y z = snd . (`runState` zeroBits) $ forM_ [0 .. 7] \i ->
	forM_ [0 .. 7] \j -> modify (flip (set i j) (calc' y z i j))
