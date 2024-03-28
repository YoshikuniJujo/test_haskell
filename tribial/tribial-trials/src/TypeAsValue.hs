{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeAsValue where

import GHC.TypeNats
import Data.Proxy

youCanAdd :: IO ()
youCanAdd =
	getNumAsType \(_ :: Proxy x) ->
	getNumAsType \(_ :: Proxy y) ->
	let	xs = replicate (fromIntegral $ natVal (Proxy :: Proxy x)) 'x'
		ys = replicate (fromIntegral $ natVal (Proxy :: Proxy y)) 'y' in
	print . length $ xs ++ ys

getNumAsType :: (forall n . KnownNat n => Proxy n -> IO a) -> IO a
getNumAsType f = do
	n <- read <$> getLine
	($ someNatVal n) \(SomeNat p) -> f p
