{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuOverridableConstant where

import GHC.JS.Prim
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import Control.Monad.ST

import Data.Word
import Data.Int

newtype G = G { unC :: C_ } deriving Show

type C_ = [(Key, Value)]

type Key = Either Int String

fromKey :: Key -> JSVal
fromKey = toJSString . either show id

data Value = Bool Bool | I32 Int32 | U32 Word32 | F32 Float deriving Show

valueToDouble :: Value -> Double
valueToDouble = \case
	Bool False -> 0; Bool True -> 1;
	I32 i -> fromIntegral i; U32 w -> fromIntegral w
	F32 f -> realToFrac f

fromValue :: Value -> JSVal
fromValue = toJSDouble . valueToDouble

foreign import javascript "((o) => { return o; })" toJSDouble :: Double -> JSVal

set1 :: JS.Object.ST s -> Key -> Value -> ST s ()
set1 o (either show id -> k) (valueToDouble -> v) = JS.Object.set o k v

toSTObject :: C_ -> ST s (JS.Object.ST s)
toSTObject c = do
	o <- JS.Object.new
	uncurry (set1 o) `mapM_` c
	pure o

toObject :: C_ -> JS.Object.O
toObject c = runST $ JS.Object.freeze =<< toSTObject c

instance JS.Value.IsJSVal G where toJSVal = JS.Value.toJSVal . toObject . unC
instance JS.Value.V G where toV = JS.Object.toValue; fromV = JS.Object.fromValue
