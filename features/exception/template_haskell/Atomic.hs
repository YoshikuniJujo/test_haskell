{-# LANGUAGE TemplateHaskell #-}

module Atomic (atomicException) where

import Control.Applicative
import Control.Exception
import Data.Typeable
import Language.Haskell.TH

atomicException :: String -> DecsQ
atomicException en = sequence [
	newtypeD (cxt [])
		(mkName en) []
		(normalC (mkName en)
			[strictType notStrict (conT ''String)])
		[''Typeable, ''Show],
	instanceD (cxt [])
		(conT ''Exception `appT` conT (mkName en))
		[]
	]
