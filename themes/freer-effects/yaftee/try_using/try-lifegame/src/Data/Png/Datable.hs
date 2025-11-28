{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Png.Datable (

	Datable(..),

	) where

import Data.ByteString.FingerTree qualified as BSF
import Data.Png.Header.Data qualified as Header

class Datable a where
	isDat :: a -> Bool
	endDat :: a -> Bool
	toDat :: Header.Header -> a -> BSF.ByteString
