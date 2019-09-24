{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples where

import Eff
import Reader

readerSample :: Eff '[Reader Integer] Integer
readerSample = do
	e <- ask
	return $ 3 * e
