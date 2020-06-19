{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module MonadicFrp.Run (
	-- * Types
	Sig, React, Handle, HandleSt,
	-- * Run
	interpret, interpretSt, interpretReact, interpretReactSt
	) where

import MonadicFrp.Sig (Sig, interpret, interpretSt)
import MonadicFrp.React (React, Handle, HandleSt, interpretReact, interpretReactSt)
