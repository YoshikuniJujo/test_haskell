{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Reflection where

import Control.Exception

class Reflection t where
	reflect :: t a -> IO a
	reify :: IO a -> IO (t a)

data Exn a = Value a | Exn String deriving Show

instance Reflection Exn where
	reflect (Value a) = pure a
	reflect (Exn e) = error e
	reify io = (Value <$> io) `catch` \(ErrorCall em) -> pure $ Exn em

handle :: IO a -> (String -> IO a) -> IO a
handle io h = reify io >>= \case
	Value v -> pure v
	Exn em -> h em
