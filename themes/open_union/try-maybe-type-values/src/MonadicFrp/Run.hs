{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module MonadicFrp.Run (
	-- * Types
	Sig, React, Handle, HandleSt,
	-- * Run
	interpret, interpretSt, interpretReact, interpretReactSt
	) where

import Prelude hiding (map, repeat, scanl, until)

import MonadicFrp.Sig
import MonadicFrp.React
