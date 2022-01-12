{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TrySwizzleFun where

import Language.Haskell.TH

import SwizzleFun

{-
do	runIO . print =<< swizzleClassPkg
	pure []
	-}

foo = $(funY) @(_, _, _)
