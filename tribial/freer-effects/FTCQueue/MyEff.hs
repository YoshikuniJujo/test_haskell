{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MyEff (
	Eff, Member, run, runM, send, handleRelay, handleRelayS, interpose
	) where

import MyEff.Internal (
	Eff, Member, run, runM, send, handleRelay, handleRelayS, interpose )
