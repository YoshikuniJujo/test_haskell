{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuColorAttachmentObject where

import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.GpuTexture qualified as JS.GpuTexture

import Control.Monad.ST

data G = G {
	loadOp :: LoadOp,
	storeOp :: StoreOp,
	view :: JS.GpuTexture.G }

instance JS.Value.IsJSVal G where
	toJSVal g = JS.Value.toJSVal $ runST $ JS.Object.freeze =<< toObject g

instance JS.Value.V G where toV = JS.Object.toValue; fromV = JS.Object.fromValue

toObject :: G -> ST s (JS.Object.ST s)
toObject g = do
	o <- JS.Object.new
	foo <- JS.Object.new
	JS.Object.set foo "r" (0 :: Float)
	JS.Object.set foo "g" (0.5 :: Float)
	JS.Object.set foo "b" (1 :: Float)
	JS.Object.set foo "a" (1 :: Float)
	JS.Object.set o "clearValue" [0 :: Float, 0.5, 1, 1]
--	JS.Object.set o "clearValue" =<< JS.Object.freeze foo -- [0 :: Float, 0.5, 1, 1]
	JS.Object.set o "loadOp" $ loadOp g
	JS.Object.set o "storeOp" $ storeOp g
	JS.Object.set o "view" $ view g
	pure o

data LoadOp = Clear | Load deriving Show
data StoreOp = Discard | Store deriving Show

instance JS.Value.IsJSVal LoadOp where
	toJSVal = JS.Value.toJSVal . \case
		Clear -> "clear"; Load -> "load"

instance JS.Value.IsJSVal StoreOp where
	toJSVal = JS.Value.toJSVal . \case
		Discard -> "discard"; Store -> "store"

instance JS.Value.V LoadOp
instance JS.Value.V StoreOp
